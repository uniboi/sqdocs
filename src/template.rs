pub struct SidebarSection<'a> {
    pub title: &'a str,
    pub headers: Vec<SidebarHeader<'a>>,
}

pub struct SidebarHeader<'a> {
    pub content: &'a str,
    pub location: String,
}

pub fn get_head(page: &str, mod_name: &str, depth: usize) -> String {
    let pre = "../".repeat(depth);
    format!("<head><title>{page} - {mod_name}</title><link rel=\"stylesheet\" href=\"{pre}resource/shared.css\"></head>")
}

pub fn get_sidebar(sections: Vec<SidebarSection>) -> String {
    format!("<nav class=\"sidebar\"><div class=\"sidebar-logo-container\"><img src=\"../../../resource/nut.png\"></div><div class=\"sidebar-elems\">{}</div></nav>",
	sections.iter().map(|section| get_sidebar_rep(section)).collect::<String>())
}

pub fn get_declaration_block(rep: &String) -> String {
    format!("<pre class=\"code-block\"><code>{rep}</code></pre>")
}

pub fn get_description_block(inner: &String) -> String {
    format!("<details open><summary>Expand description</summary>{inner}</details>",)
}

pub fn wrap_html(head: &String, body: &String) -> String {
    format!("<!DOCTYPE html><html>{head}{body}</html>")
}

pub fn get_body(sidebar: &String, title: &String, block: &String, description: &String) -> String {
    format!("<body>{sidebar}<main><h1>{title}</h1>{block}{description}</main></body>")
}

fn get_sidebar_rep(section: &SidebarSection) -> String {
    format!(
        "<h3>{}</h3><ul class=\"sidebar-block\">{}</ul>",
        section.title,
        section
            .headers
            .iter()
            .map(|header| format!(
                "<li><a href=\"{}\" title=\"{}\">{}</a></li>",
                header.location, header.content, header.content
            ))
            .collect::<String>()
    )
}
