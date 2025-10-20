use std::path::Path;

fn main() {
    if std::env::var_os("SKIP_TAILWIND").is_none() {
        println!("cargo::warning=Building tailwind");
        build_tailwind();
        println!("cargo::warning=Done building Tailwind");
    }
    println!("cargo::rerun-if-changed=src/");
    println!("cargo::rerun-if-changed=tailwind/");
}

pub fn build_tailwind() {
    let target = std::env::var_os("OUT_DIR").unwrap();
    let target_dir = Path::new(&target).parent().unwrap().parent().unwrap();
    let res = std::process::Command::new("tailwindcss")
        .arg("--content")
        .arg(format!(
            "{}/**/*.rs,./src/**/*.{{html,rs}},./index.html",
            target_dir.display(),
        ))
        .arg("-i")
        .arg("./tailwind/input.css")
        .arg("-o")
        .arg("./static/tailwind.css")
        .arg("--minify")
        .status()
        .expect("Could not execute");
    assert!(res.success());
}
