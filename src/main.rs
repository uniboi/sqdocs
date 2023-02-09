mod rep;

use std::{ffi::OsString, fs, io, io::Write};

use serde::{Deserialize, Serialize};
use sqparse::ast::{FunctionDeclarationStatement, StatementType, StructProperty, Type};
use sqparse::token::TokenLine;
use sqparse::{parse, tokenize};

#[derive(Serialize, Deserialize)]
struct Mod {
    Name: String,
    Description: String,
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
    properties: Vec<StructProperty<'a>>,
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path = args.get(1).expect("no path to directory was provided");

    // generate_all_docs(OsString::from(path)).unwrap();
    generate_docs_for_mods(OsString::from(path)).unwrap();
}

fn get_head(page: &str, mod_name: &str) -> String {
    format!("<head><title>{page} - {mod_name}</title><link rel=\"stylesheet\" href=\"../resource/shared.css\"></head>")
}

fn generate_docs_for_mods(path: OsString) -> io::Result<()> {
    let mut sub_directories = Vec::<std::path::PathBuf>::new();
    let mut found_mod = false;

    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let mut path = entry.path();
        if path.is_dir() {
            sub_directories.push(path);
        } else if entry.file_name() == "mod.json" {
            let mod_json = fs::read_to_string(path.clone())?;
            let m: Mod = serde_json::from_str(&mod_json)?;

            println!("generating docs for {}", m.Name);

            path.pop();
            // println!("located mod root at {:#?}", path);
            path.push("mod/scripts/vscripts/");
            found_mod = true;
            generate_docs_for_mod(path.into(), m.Name)?;
        }
    }
    if !found_mod {
        for path in sub_directories {
            generate_docs_for_mods(path.into())?;
        }
    }
    Ok(())
}

fn generate_docs_for_mod(path: OsString, mod_name: String) -> io::Result<()> {
    let mut document_all_methods = false;
    let mut expected_methods = std::collections::HashSet::<&str>::new();

    let scripts = get_all_scripts(&path)?;

    let mut files = Vec::new();
    let mut file_tokens = Vec::new();
    let mut documented_functions = Vec::new();

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
                        println!("{:#?}", d);
                        documented_functions.push(Box::leak(Box::new(FunctionInfo {
                            decl: d.clone(),
                            identifier: d.name.last_item.value,
                            description: get_function_comments(d),
                        })));
                    }
                }
                StatementType::GlobalizeAllFunctions(_) => {
                    document_all_methods = true;
                }
                StatementType::Global(d) => match &d.declaration {
                    sqparse::ast::GlobalDeclaration::Function { function: _, name } => {
                        expected_methods.insert(name.last_item.value);
                        ()
                    }
                    _ => (),
                },
                _ => (),
            }
        }
    }

    let sidebar = format!(
    	"<nav class=\"sidebar\"><div class=\"sidebar-logo-container\"><img src=\"../resource/nut.png\"></div><div class=\"sidebar-elems\"><h3>Functions</h3><ul class=\"sidebar-block\">{}</ul><h3>Structs</h3><ul class=\"sidebar-block\">{}</ul></div></nav>",
    	documented_functions.iter().map(|f|format!("<li><a href=\"./fn_{}.html\" title=\"temp\">{}</a></li>", f.identifier,f.identifier)).collect::<String>(),
    	// documented_structs.iter().map(|s|format!("<li><a href=\"./st_{}.html\">{}</a></li>",s.identifier ,s.identifier)).collect::<String>()
    	""
    );

    for v in documented_functions {
        // println!("generating docs for function {}", v.identifier);
        write_function_html(v, &sidebar, &mod_name)?;
    }

    Ok(())
}

fn get_all_scripts(path: &OsString) -> io::Result<Vec<std::path::PathBuf>> {
    let mut scripts = Vec::<std::path::PathBuf>::new();

    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            scripts.append(&mut get_all_scripts(&path.into_os_string())?);
        } else if path.extension().unwrap() == "nut" || path.extension().unwrap() == "gnut" {
            scripts.push(path);
        }
    }

    Ok(scripts)
}

fn write_function_html(f: &FunctionInfo, sidebar: &String, mod_name: &String) -> std::io::Result<()> {
    let mut file = fs::File::create(format!("out/fn_{}.html", f.identifier))?;
    write!(file, "{}", get_function_representation(f, sidebar, mod_name))?;
    Ok(())
}

fn get_function_representation(f: &FunctionInfo, sidebar: &String, mod_name: &String) -> String {
    let head = get_head(f.identifier, mod_name);
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
                    sqparse::token::Comment::MultiLine(c) => markdown::to_html(
                        &c.split("\n")
                            .map(|s| s.trim())
                            .collect::<Vec<_>>()
                            .join("\n")
                    ),
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
