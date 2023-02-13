mod rep;

use std::path::PathBuf;
use std::{ffi::OsString, fs, io, io::Write};

use serde::{Deserialize, Serialize};
use sqparse::ast::{
    FunctionDeclarationStatement, StatementType, StructDeclarationStatement, StructProperty, Type,
};
use sqparse::token::TokenLine;
use sqparse::{parse, tokenize};

#[derive(Serialize, Deserialize)]
struct ModManifest {
    Name: String,
    Description: String,
}

struct Mod {
    manifest: ModManifest,
    path: OsString,
}

#[derive(Debug, Clone)]
struct FunctionInfo<'a> {
    identifier: &'a str,
    // return_type: Option<Type<'a>>,
    // args: FunctionArgs<'a>,
    decl: FunctionDeclarationStatement<'a>,
    // args: Vec<FunctionArg<'a>>,
    // function: &'a FunctionDeclarationStatement<'a>,
    description: String,
}

#[derive(Debug, Clone)]
struct StructInfo<'a> {
    identifier: &'a str,
    decl: StructDeclarationStatement<'a>,
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path = args.get(1).expect("no path to directory was provided");

    fs::remove_dir_all("out").unwrap();
    fs::create_dir("out").unwrap();

    let mods = generate_docs_for_mods(OsString::from(path)).unwrap();
    generate_collection_page(mods).unwrap();
}

fn get_head(page: &str, mod_name: &str, depth: usize) -> String {
    let pre = "../".repeat(depth);
    format!("<head><title>{page} - {mod_name}</title><link rel=\"stylesheet\" href=\"{pre}resource/shared.css\"></head>")
}

fn generate_docs_for_mods(path: OsString) -> io::Result<Vec<Mod>> {
    let mods = get_all_mods_in_dir(&path)?;

    for m in &mods {
        generate_docs_for_mod(
            &m.path,
            &m.manifest,
            mods.iter().map(|m: &Mod| &m.manifest.Name).collect(),
        )?;
    }

    Ok(mods)
}

fn get_all_mods_in_dir(path: &OsString) -> io::Result<Vec<Mod>> {
    let mut sub_directories = Vec::<std::path::PathBuf>::new();
    let mut found_mod = false;

    let mut mods = Vec::new();

    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let mut path = entry.path();
        if path.is_dir() {
            sub_directories.push(path);
        } else if entry.file_name() == "mod.json" {
            let mod_json = fs::read_to_string(path.clone())?;
            let mf: ModManifest = serde_json::from_str(&mod_json)?;

            println!("generating docs for {}", mf.Name);

            path.pop();
            // println!("located mod root at {:#?}", path);
            path.push("mod/scripts/vscripts/");
            found_mod = true;
            mods.push(Mod {
                manifest: mf,
                path: OsString::from(path),
            });
        }
    }
    if !found_mod {
        for path in sub_directories {
            mods.append(&mut get_all_mods_in_dir(&path.into())?);
        }
    }

    Ok(mods)
}

fn generate_collection_page(mods: Vec<Mod>) -> io::Result<()> {
    let mut file = fs::File::create("out/collection.html")?;
    let head = get_head("Index", "Collection", 1);
    let mod_list = mods
        .iter()
        .map(|m| {
            format!(
                "<li><a href=\"./{}/mod/mod.html\">{}</a></li>",
                m.manifest.Name, m.manifest.Name
            )
        })
        .collect::<Vec<_>>()
        .join("");
    let body = format!("<ul>{mod_list}</ul>");
    write!(
        file,
        "<!DOCTYPE html><html>{head}<body>{body}</body></html>",
    )?;
    Ok(())
}

fn generate_docs_for_mod(
    path: &OsString,
    m: &ModManifest,
    all_mod_names: Vec<&String>,
) -> io::Result<()> {
    let mut document_all_methods = false;
    let mut expected_methods = std::collections::HashSet::<&str>::new();

    let scripts = get_all_scripts(&path)?;

    let mut files = Vec::new();
    let mut file_tokens = Vec::new();
    let mut documented_functions = Vec::new();
    let mut documented_structs = Vec::new();

    for script in scripts {
        let script_content = fs::read_to_string(script)?;
        files.push(script_content);
    }

    for i in 0..files.len() {
        let tokens = tokenize(&files[i], sqparse::Flavor::SquirrelRespawn).unwrap();
        file_tokens.push(tokens);
    }

    for i in 0..file_tokens.len() {
        let ast = parse(&file_tokens[i]).unwrap();

        for statement in ast.statements {
            match statement.ty {
                StatementType::FunctionDeclaration(d) => {
                    if document_all_methods || expected_methods.contains(d.name.last_item.value) {
                        // println!("{:#?}", d);
                        documented_functions.push(FunctionInfo {
                            decl: d.clone(),
                            identifier: d.name.last_item.value,
                            description: get_function_comments(d),
                        });
                    }
                }
                StatementType::GlobalizeAllFunctions(_) => {
                    document_all_methods = true;
                }
                StatementType::Global(d) => match &d.declaration {
                    sqparse::ast::GlobalDeclaration::Function { function: _, name } => {
                        expected_methods.insert(name.last_item.value);
                    }
                    sqparse::ast::GlobalDeclaration::Struct(s) => {
                        documented_structs.push(StructInfo {
                            decl: s.clone(),
                            identifier: s.name.value,
                        })
                    }
                    _ => (),
                },
                _ => (),
            }
        }
    }

    let sidebar = format!(
        "<nav class=\"sidebar\"><div class=\"sidebar-logo-container\"><img src=\"../../../resource/nut.png\"></div><div class=\"sidebar-elems\"><h3>Mods</h3><ul class=\"sidebar-block\">{}</ul><h3>Functions</h3><ul class=\"sidebar-block\">{}</ul><h3>Structs</h3><ul class=\"sidebar-block\">{}</ul></div></nav>",
        all_mod_names.iter().map(|n|format!("<li><a href=\"../../{}/mod/mod.html\" title=\"temp\">{}</a></li>", n, n)).collect::<String>(),
        documented_functions.iter().map(|f|format!("<li><a href=\"../functions/{}.html\" title=\"temp\">{}</a></li>", f.identifier, f.identifier)).collect::<String>(),
        documented_structs.iter().map(|s|format!("<li><a href=\"../structs/{}.html\" title=\"temp\">{}</a></li>", s.identifier, s.identifier)).collect::<String>(),
    );

    fs::create_dir(format!("out/{}", m.Name))?;
    fs::create_dir(format!("out/{}/mod", m.Name))?;
    fs::create_dir(format!("out/{}/functions", m.Name))?;
    fs::create_dir(format!("out/{}/structs", m.Name))?; // TODO
    fs::create_dir(format!("out/{}/enums", m.Name))?; // TODO
    fs::create_dir(format!("out/{}/globals", m.Name))?; // TODO

    for v in documented_functions {
        // println!("generating docs for function {}", v.identifier);
        write_function_html(&v, &sidebar, &m.Name)?;
    }

    write_mod_index(&sidebar, m)?;

    Ok(())
}

fn get_all_scripts(path: &OsString) -> io::Result<Vec<std::path::PathBuf>> {
    let mut scripts = Vec::<std::path::PathBuf>::new();

    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            scripts.append(&mut get_all_scripts(&path.into_os_string())?);
        } else if path.extension().unwrap_or(&OsString::new()) == "nut"
            || path.extension().unwrap_or(&OsString::new()) == "gnut"
        {
            scripts.push(path);
        }
    }

    Ok(scripts)
}

fn write_mod_index(sidebar: &String, m: &ModManifest) -> io::Result<()> {
    let mut file = fs::File::create(format!("out/{}/mod/mod.html", m.Name))?;
    let head = get_head("Index", &m.Name, 3);
    write!(
        file,
        "<!DOCTYPE html><html>{head}<body>{sidebar}<main><h1>{}</h1>{}</main></body></html>",
        m.Name,
        markdown::to_html(&m.Description)
    )?;
    Ok(())
}

fn write_function_html(
    f: &FunctionInfo,
    sidebar: &String,
    mod_name: &String,
) -> std::io::Result<()> {
    let mut file = fs::File::create(format!("out/{mod_name}/functions/{}.html", f.identifier))?;
    write!(
        file,
        "{}",
        get_function_representation(f, sidebar, mod_name)
    )?;
    Ok(())
}

fn get_function_representation(f: &FunctionInfo, sidebar: &String, mod_name: &String) -> String {
    let head = get_head(f.identifier, mod_name, 3);
    let rep = html_escape::encode_text(&format!(
        "{} function {}({})",
        rep::get_type_rep(&f.decl.return_type, 0),
        f.identifier,
        rep::get_function_param_rep(&f.decl.declaration.args, 0),
    ))
    .to_string();
    let block = format!("<pre class=\"code-block\"><code>{rep}</code></pre>");
    let description = format!(
        "<details open><summary>Expand description</summary>{}</details>",
        f.description
    );
    let body = format!(
        "<body>{sidebar}<main><h1>{}</h1>{block}{description}</main></body>",
        f.identifier,
    );
    format!("<!DOCTYPE html><html>{head}{body}</html>")
}

fn get_function_comments(d: FunctionDeclarationStatement) -> String {
    format_comments(match d.return_type {
        Some(ty) => get_type_comments(ty),
        None => &d.function.before_lines,
    })
}

fn get_type_comments(ty: Type) -> &Vec<TokenLine> {
    match ty {
        Type::Local(ty) => &ty.local.before_lines,
        Type::Var(ty) => &ty.var.before_lines,
        Type::Plain(ty) => &ty.name.token.before_lines,
        Type::Array(ty) => get_type_comments(*ty.base),
        Type::Generic(ty) => get_type_comments(*ty.base),
        Type::FunctionRef(ty) => match ty.return_type {
            Some(ty) => get_type_comments(*ty),
            None => &ty.functionref.before_lines,
        },
        Type::Struct(_) => todo!(),
        Type::Reference(ty) => get_type_comments(*ty.base),
        Type::Nullable(ty) => get_type_comments(*ty.base),
    }
}

fn format_comments(comments: &Vec<TokenLine>) -> String {
    let no_desc = String::from("<p>No description available</p>");
    format!(
        "<div class=\"description\">{}</div>",
        match comments.iter().last() {
            Some(line) => match line.comments.last() {
                Some(comment) => match comment {
                    sqparse::token::Comment::MultiLine(c) => {
                        let mut comments = c.split("\n");
                        // let content_index;
                        // let prefix;
                        // let first = comments.nth(0).unwrap_or(&no_desc).as_bytes();

                        // for i in 0..first.len() {
                        //     let c = first.get(i).unwrap();
                        //     if c == &(' ' as u8) || c == &('\t' as u8) || c == &('*' as u8) {
                        //         continue;
                        //     }
                        //     content_index = i;
                        //     prefix = first[..i].join("");
                        //     break;
                        // }

                        markdown::to_html(
                            &c.split("\n")
                                .map(|s| s.trim())
                                .collect::<Vec<_>>()
                                .join("\n"),
                        )
                    }
                    sqparse::token::Comment::SingleLine(c) => {
                        let t = c.trim();
                        String::from(if t.len() > 0 { t } else { c })
                    }
                    sqparse::token::Comment::ScriptStyle(_) => no_desc,
                },
                None => no_desc,
            },
            None => no_desc,
        }
    )
}
