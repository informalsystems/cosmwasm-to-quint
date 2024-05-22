use std::{env, fs, io, path::Path, process::Command, sync::Once};

use anyhow::{ensure, Context, Result};

static SETUP: Once = Once::new();

fn run(dir: &str, f: impl FnOnce(&mut Command)) -> Result<String> {
    let root = env::temp_dir().join("cosmwasm-to-quint");

    let heredir = Path::new(".").canonicalize()?;

    SETUP.call_once(|| {
        let mut cmd = Command::new("cargo");
        cmd.args(["install", "--path", ".", "--debug", "--locked", "--root"]);
        cmd.arg(&root);
        cmd.current_dir(&heredir);
        let status = cmd.status().unwrap();
        if !status.success() {
            panic!("installing cosmwasm-to-quint failed")
        }
    });

    let mut cmd = Command::new("cargo");
    cmd.arg("cosmwasm-to-quint");

    let path = format!(
        "{}:{}",
        root.join("bin").display(),
        env::var("PATH").unwrap_or_else(|_| "".into())
    );
    cmd.env("PATH", path);

    let ws = heredir.join("tests").join("fixtures").join(dir);
    cmd.current_dir(&ws);

    f(&mut cmd);

    let _ = fs::remove_dir_all(ws.join("target"));

    let output = cmd.output().context("Process failed")?;
    ensure!(
        output.status.success(),
        "Process exited with non-zero exit code. Stderr:\n{}",
        String::from_utf8(output.stderr)?
    );

    let mut quint_entries = fs::read_dir(ws.join("quint"))?
        .map(|res| res.map(|e| e.path()))
        .collect::<Result<Vec<_>, io::Error>>()?;
    quint_entries.sort();

    let quint_files = quint_entries
        .iter()
        .map(|path| {
            if path.is_dir() {
                return "".to_string();
            }
            format!(
                "{}:\n{}",
                path.clone().file_name().unwrap().to_string_lossy(),
                fs::read_to_string(path).expect("Should have been able to read the file"),
            )
        })
        .collect::<Vec<String>>()
        .join("\n\n");

    let mut mbt_entries = fs::read_dir(ws.join("src/mbt"))?
        .map(|res| res.map(|e| e.path()))
        .collect::<Result<Vec<_>, io::Error>>()?;
    mbt_entries.sort();

    let mbt_files = mbt_entries
        .iter()
        .map(|path| {
            if path.is_dir() {
                return "".to_string();
            }
            format!(
                "{}:\n{}",
                path.clone().file_name().unwrap().to_string_lossy(),
                fs::read_to_string(path).expect("Should have been able to read the file"),
            )
        })
        .collect::<Vec<String>>()
        .join("\n\n");

    Ok(format!("quint:\n{}\n\nmbt:\n{}", quint_files, mbt_files))
}

#[test]
fn ctf01() -> Result<()> {
    let output = run("cosmwasm-ctf/ctf-01", |_cmd| {})?;
    insta::assert_snapshot!(output);
    Ok(())
}

#[test]
fn ctf02() -> Result<()> {
    let output = run("cosmwasm-ctf/ctf-02", |_cmd| {})?;
    insta::assert_snapshot!(output);
    Ok(())
}

#[test]
fn ctf03() -> Result<()> {
    let output = run("cosmwasm-ctf/ctf-03", |_cmd| {})?;
    insta::assert_snapshot!(output);
    Ok(())
}

#[test]
fn ctf04() -> Result<()> {
    let output = run("cosmwasm-ctf/ctf-04", |_cmd| {})?;
    insta::assert_snapshot!(output);
    Ok(())
}

#[test]
fn ctf05() -> Result<()> {
    let output = run("cosmwasm-ctf/ctf-05", |_cmd| {})?;
    insta::assert_snapshot!(output);
    Ok(())
}

#[test]
fn ctf06() -> Result<()> {
    let output = run("cosmwasm-ctf/ctf-06", |_cmd| {})?;
    insta::assert_snapshot!(output);
    Ok(())
}

#[test]
fn ctf07() -> Result<()> {
    let output = run("cosmwasm-ctf/ctf-07", |_cmd| {})?;
    insta::assert_snapshot!(output);
    Ok(())
}

#[test]
fn ctf08() -> Result<()> {
    let output = run("cosmwasm-ctf/ctf-08", |_cmd| {})?;
    insta::assert_snapshot!(output);
    Ok(())
}

#[test]
fn ctf09() -> Result<()> {
    let output = run("cosmwasm-ctf/ctf-09", |_cmd| {})?;
    insta::assert_snapshot!(output);
    Ok(())
}

#[test]
fn ctf10() -> Result<()> {
    let output = run("cosmwasm-ctf/ctf-10", |_cmd| {})?;
    insta::assert_snapshot!(output);
    Ok(())
}
