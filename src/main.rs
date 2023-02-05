mod rep;

use std::ffi::OsStr;
use std::path::Path;
use std::{ffi::OsString, fs, io, io::Write};

use sqparse::ast::{FunctionArg, FunctionArgs, FunctionDeclarationStatement, StatementType, Type};
use sqparse::token::TokenLine;
use sqparse::{parse, tokenize};

#[derive(Debug, Clone)]

struct FunctionInfo<'a> {
    return_type: Option<Type<'a>>,
    identifier: &'a str,
    args: FunctionArgs<'a>,
    // args: Vec<FunctionArg<'a>>,
    // function: &'a FunctionDeclarationStatement<'a>,
    description: String,
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path = args.get(1).expect("no path to directory was provided");

    generate_all_docs(OsString::from(path)).unwrap();
}

fn get_extension_from_filename(filename: &str) -> Option<&str> {
    Path::new(filename).extension().and_then(OsStr::to_str)
}

fn generate_all_docs(path: OsString) -> io::Result<()> {
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            generate_all_docs(path.into())?;
        } else {
            let extension = get_extension_from_filename(path.to_str().unwrap());
            if extension == Some("nut") || extension == Some("gnut") {
                generate_docs_for_file(path.into());
            }
        }
    }
    Ok(())
}

fn generate_docs_for_file(path: OsString) {
    println!("parsing {path:?}");
    let binding = fs::read_to_string(path).expect("Failed reading file");
    let tokens = tokenize(&binding, sqparse::Flavor::SquirrelRespawn).unwrap();
    let ast = parse(&tokens).unwrap();

    let mut document_all_methods = false;
    let mut expected_methods = std::collections::HashSet::<&str>::new();
    let mut documented_methods = Vec::<FunctionInfo>::new();

    for statement in ast.statements {
        match statement.ty {
            StatementType::Global(g) => println!(
                "{}",
                match g.declaration {
                    sqparse::ast::GlobalDeclaration::Function { function: _, name } => {
                        expected_methods.insert(&*name.last_item.value);
                        format!("{}", &*name.last_item.value)
                    }
                    _ => format!("unimplemented globalization"),
                }
            ),
            StatementType::GlobalizeAllFunctions(_) => document_all_methods = true,
            StatementType::FunctionDeclaration(d) => {
                if document_all_methods || expected_methods.contains(&d.name.last_item.value) {
                    documented_methods.push(FunctionInfo {
                        identifier: d.name.last_item.value,
                        args: d.declaration.args,
                        description: format_comments(&d.function.before_lines),
                        return_type: d.return_type,
                    });
                }
                ()
            }
            StatementType::StructDeclaration(s) => {
                // add_type_def(String::from(s.name.value), TypeInfo { identifier: (), content_types: (), internal: () });
            }
            _ => (),
        }
    }

    println!("{documented_methods:#?}");
    write_functions_html(documented_methods).unwrap();
}

fn write_functions_html(fns: Vec<FunctionInfo>) -> std::io::Result<()> {
    let sidebar = format!(
        "<nav class=\"sidebar\"><div class=\"sidebar-logo-container\"><img src=\"../resource/nut.png\"></div><div class=\"sidebar-elems\"><h3>Functions</h3><ul class=\"sidebar-block\">{}</ul></div></nav>",
		fns.iter().map(|f|format!("<li><a href=\"./fn_{}.html\" title=\"temp\">{}</a></li>", f.identifier,f.identifier)).collect::<String>()
    );
    for f in fns.iter() {
        write_function_html(f, &sidebar)?;
    }
    Ok(())
}

fn write_function_html(f: &FunctionInfo, sidebar: &String) -> std::io::Result<()> {
    let mut file = fs::File::create(format!("out/fn_{}.html", f.identifier))?;
    write!(file, "{}", get_function_representation(f, sidebar))?;
    Ok(())
}

fn get_function_representation(f: &FunctionInfo, sidebar: &String) -> String {
    let head = format!(
        "<head><title>{} - ModTitle</title><link rel=\"stylesheet\" href=\"../resource/shared.css\"></head>",
        f.identifier
    );
    let rep = html_escape::encode_text(&format!(
        "{} function {}({})",
        rep::get_type_rep(&f.return_type, 0),
        f.identifier,
        rep::get_function_param_rep(&f.args, 0),
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

fn format_comments(comments: &Vec<TokenLine>) -> String {
    let no_desc = String::from("<p>No description given</p>");
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
