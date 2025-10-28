use maud::{html, DOCTYPE};

pub fn maud_page(content: maud::Markup) -> maud::Markup {
    html! {
       (DOCTYPE)
       (maud_header())
       (maud_body(content))
    }
}

fn maud_header() -> maud::Markup {
    html! {
        link rel="stylesheet" href="/static/style.css";
        script src="/static/htmx-1.9.11.js" {}
        meta name="viewport" content="width=device-width, initial-scale=1.0";
    }
}

fn maud_nav() -> maud::Markup {
    html! {
        nav class="header" {
            div class="" {
                a class="" href="/" { "Folio" }

            };
        }
    }
}

fn maud_body(content: maud::Markup) -> maud::Markup {
    html! {
        body class="body"{
            (maud_nav())
            div class="" {

                div class="" {
                        (content)
                };
            };
        };
    }
}

pub fn sidebar() -> maud::Markup {
    html! {
        div class="sidebar" {
          h2 { "Files:" }
          ul {
            li { a href="/files/upload" {"Upload"}}

          }
        }
    }
}
