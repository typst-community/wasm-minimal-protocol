use regex::Regex;
use semver::Version;
use std::{
    path::{Path, PathBuf},
    process::Command,
    sync::LazyLock,
};
use which::{which, which_re};

fn wasi_stub_with_args(path: PathBuf, extra_args: &[&str]) {
    let path = path.canonicalize().unwrap();

    let wasi_stub = Command::new("cargo")
        .arg("run")
        .arg("--")
        .args(extra_args)
        .arg(&path)
        .arg("-o")
        .arg(&path)
        .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/../wasi-stub"))
        .status()
        .unwrap();
    if !wasi_stub.success() {
        panic!("wasi-stub failed");
    }
}

fn wasi_stub(path: PathBuf) {
    wasi_stub_with_args(path, &[]);
}

/// Compile `hello.typ` in `path` with all Typst compilers that declared to be compatible.
/// Panics if the compilation fails or there is no compatible Typst compiler available.
fn typst_compile(path: &Path) {
    // Get all available Typst compilers
    static TYPST_COMPILERS: LazyLock<Vec<(PathBuf, Version)>> = LazyLock::new(|| {
        std::iter::once(which("typst").expect("the latest typst compiler should be available"))
            .chain(
                which_re(Regex::new("^typst-.*").expect("a valid regex"))
                    .expect("which_re should work"),
            )
            .map(|executable| {
                let typst_version = Command::new(&executable).arg("--version").output().unwrap();
                if !typst_version.status.success() {
                    panic!("{executable:?} --version failed");
                }
                let version_output = match String::from_utf8(typst_version.stdout) {
                    Ok(s) => s,
                    Err(err) => panic!("failed to parse typst version: {err}"),
                };
                let version = version_output
                    .split_whitespace()
                    .nth(1)
                    .and_then(|v| Version::parse(v).ok())
                    .expect("version output should be in the format `typst X.Y.Z (COMMIT_HASH)`");
                (executable, version)
            })
            .collect()
    });

    let path = PathBuf::from(path).canonicalize().unwrap();

    // Parse minimum supported Typst version from `hello.typ`
    let min_compiler = std::fs::read_to_string(path.join("hello.typ"))
        .expect("should read hello.typ")
        .lines()
        .next()
        .and_then(|line| line.strip_prefix("//! Minimum supported Typst version: "))
        .and_then(|v| Version::parse(v).ok())
        .expect(
            "The first line of `hello.typ` should document the minimum supported Typst version",
        );

    // Filter compatible compilers
    let compatible_compilers: Vec<_> = TYPST_COMPILERS
        .iter()
        .filter_map(|(compiler, version)| (*version >= min_compiler).then_some(compiler))
        .collect();
    if compatible_compilers.is_empty() {
        panic!("No compatible Typst compiler found. Minimum supported version is {min_compiler}, but found: {TYPST_COMPILERS:?}");
    }

    // Call compilers
    let failed: Vec<_> = compatible_compilers
        .iter()
        .filter_map(|compiler| {
            let typst_compile = Command::new(compiler)
                .arg("compile")
                .arg("hello.typ")
                .current_dir(&path)
                .status();

            if typst_compile.is_ok_and(|status| status.success()) {
                None
            } else {
                Some(format!("{compiler:?} compile failed"))
            }
        })
        .collect();
    if !failed.is_empty() {
        panic!("Compilation failed: {failed:#?}");
    }
}

#[test]
fn test_c() {
    let dir_path = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../examples/hello_c"
    ));

    let build_c = Command::new("emcc")
        .arg("--no-entry")
        .arg("-O3")
        .arg("-s")
        .arg("ERROR_ON_UNDEFINED_SYMBOLS=0")
        .arg("-o")
        .arg("hello.wasm")
        .arg("hello.c")
        .current_dir(dir_path)
        .status()
        .unwrap();
    if !build_c.success() {
        panic!("Compiling with emcc failed");
    }
    wasi_stub(dir_path.join("hello.wasm"));
    typst_compile(dir_path);
}

#[test]
fn test_rust() {
    let dir_path = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../examples/hello_rust"
    ));

    for target in ["wasm32-unknown-unknown", "wasm32-wasip1"] {
        let build_rust = Command::new("cargo")
            .arg("build")
            .arg("--release")
            .arg("--target")
            .arg(target)
            .current_dir(dir_path)
            .status()
            .unwrap();
        if !build_rust.success() {
            panic!("Compiling with cargo failed");
        }
        std::fs::copy(
            dir_path
                .join("target")
                .join(target)
                .join("release/hello.wasm"),
            dir_path.join("hello.wasm"),
        )
        .unwrap();
        if target == "wasm32-wasip1" {
            wasi_stub(dir_path.join("hello.wasm"));
        }
        typst_compile(dir_path);
    }
}

#[test]
fn test_zig() {
    let dir_path = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../examples/hello_zig"
    ));

    for target in ["wasm32-freestanding", "wasm32-wasi"] {
        let build_zig = Command::new("zig")
            .arg("build-exe")
            .arg("hello.zig")
            .arg("-target")
            .arg(target)
            .arg("-fno-entry")
            .arg("-O")
            .arg("ReleaseSmall")
            .arg("--export=hello")
            .arg("--export=double_it")
            .arg("--export=concatenate")
            .arg("--export=shuffle")
            .arg("--export=returns_ok")
            .arg("--export=returns_err")
            .arg("--export=will_panic")
            .current_dir(dir_path)
            .status()
            .unwrap();
        if !build_zig.success() {
            panic!("Compiling with zig failed");
        }
        if target == "wasm32-wasi" {
            wasi_stub(dir_path.join("hello.wasm"));
        }
        typst_compile(dir_path);
    }
}

#[test]
fn test_go() {
    let dir_path = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../examples/hello_go"
    ));

    let build_go = Command::new("tinygo")
        .arg("build")
        .arg("-target=wasm-unknown")
        .arg("-o")
        .arg("hello.wasm")
        .current_dir(dir_path)
        .status()
        .unwrap();
    if !build_go.success() {
        panic!("Compiling with tinygo for wasm-unknown failed");
    }
    typst_compile(dir_path);

    let build_go_wasi = Command::new("tinygo")
        .arg("build")
        .arg("-o")
        .arg("hello.wasm")
        .env("GOOS", "wasip1")
        .env("GOARCH", "wasm")
        .current_dir(dir_path)
        .status()
        .unwrap();
    if !build_go_wasi.success() {
        panic!("Compiling with tinygo for wasip1 failed");
    }
    wasi_stub(dir_path.join("hello.wasm"));
    typst_compile(dir_path);
}

#[test]
fn test_haskell() {
    let dir_path = Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../examples/hello_haskell"
    ));

    let export_flags = {
        let mut exports = vec!["hs_init_wrapped"];
        // Extract exported function names from Main.hs
        let main_hs = std::fs::read_to_string(dir_path.join("Main.hs")).unwrap();
        for line in main_hs.lines() {
            if let Some(func_name) =
                line.trim()
                    .strip_prefix("foreign export ccall")
                    .and_then(|f| {
                        f.trim()
                            .strip_prefix("\"")
                            .and_then(|s| s.strip_suffix("\""))
                    })
            {
                exports.push(func_name);
            }
        }
        exports
            .iter()
            .map(|e| format!("--export={}", e))
            .collect::<Vec<_>>()
            .join(",")
    };

    let build_haskell = Command::new("wasm32-wasi-ghc")
        .arg("Main.hs")
        .arg("typ_wrapper.c")
        .arg("-o")
        .arg("hello.wasm")
        .arg(format!("-optl-Wl,{export_flags}"))
        .current_dir(dir_path)
        .status()
        .unwrap();
    if !build_haskell.success() {
        panic!("Compiling with GHC WebAssembly backend failed");
    }
    wasi_stub_with_args(dir_path.join("hello.wasm"), &["--return-value", "0"]);
    typst_compile(dir_path);
}
