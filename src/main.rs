use axum::{
    http::StatusCode,
    response::{IntoResponse, Html},
    routing::{get, post},
    Router,
    extract::Path,
    Json
};
use serde::{Serialize, Deserialize};
use serde_json::Value;
use reqwest::{
    Error,
    Client
};

use dotenv::dotenv;

#[derive(Serialize)]
struct Greeting {
    greet: String,
    name: String,
}

#[derive(Deserialize)]
struct CustGreet {
    input: String,
    name: String,
}

#[derive(Serialize)]
struct Message {
    msg: String
}

#[derive(Serialize)]
struct AnnouncementShort {
    date: String,
    title: String,
    content: String
}

async fn handler_404() -> impl IntoResponse {
    (StatusCode::NOT_FOUND, "404 - page not Found")
}

async fn wrong_request(r: &str) -> impl IntoResponse {
    (StatusCode::BAD_REQUEST, "400 - Wrong request, wanted ".to_owned() + &String::from(r))
}

async fn greet_name(Path(greet_name): Path<String>) -> Json<Greeting> {
    let greeting = Greeting { greet: String::from("Hello"), name: greet_name};
    Json(greeting)
}

async fn greet_cust(Json(custGreet): Json<CustGreet>) -> Json<Message> {
    let message = Message { msg: custGreet.input + " " + &custGreet.name};
    Json(message)
}

async fn announcements() -> Result<Json<Vec<AnnouncementShort>>, Error> {
    dotenv().ok();
    let token = std::env::var("TOKEN").expect("TOKEN must be set.");
    let client = Client::new();
    
    let res = client
        .get("https://git.gvk.idi.ntnu.no/api/v4/projects/5881/issues?labels=Announcement&state=opened")
        .bearer_auth(token)
        .send()
        .await?;
    
    let json: Value = res.json().await?;

    let mut ann = Vec::new();
    if let Some(array) = json.as_array() {
        for item in array {
            if let Some(obj) = item.as_object() {
                ann.push(AnnouncementShort {
                    date: obj.get("created_at").and_then(|v| v.as_str()).unwrap_or_default().to_string(),
                    title: obj.get("title").and_then(|v| v.as_str()).unwrap_or_default().to_string(),
                    content: obj.get("description").and_then(|v| v.as_str()).unwrap_or_default().to_string(),
                });
            }
        }
    }
    
    ann.sort_by_key(|a| a.date.clone());

    println!("{}", serde_json::to_string_pretty(&ann).unwrap());

    Ok(Json(ann))
}

async fn announcements_handler_json() -> impl IntoResponse {
    match announcements().await {
        Ok(json) => (StatusCode::OK, json).into_response(),
        Err(e) => (StatusCode::INTERNAL_SERVER_ERROR, format!("Error: {}", e)).into_response(),
    }
}

async fn announcements_handler_html() -> impl IntoResponse {
    match announcements().await {
        Ok(Json(ann_list)) => {
            let mut html = String::from("<html><head><title>Course Announcements</title></head><body>");
            html.push_str("<h1>Course Announcements</h1><ul>");
            for ann in ann_list {
                html.push_str(&format!(
                    "<li>{}<br><em>{}</em><p>{}</p></li>",
                    ann.title, ann.date, ann.content
                ));
            }
            html.push_str("</ul></body></html>");

            Html(html).into_response()
        },
        Err(e) => (StatusCode::INTERNAL_SERVER_ERROR, format!("Error: {}", e)).into_response(),
    }
}


#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/hello", get(|| async { "Hello, World!" })
                        .post(|| async { wrong_request("GET").await }))
        .route("/greet/:name", get(greet_name)
                              .post(|| async { wrong_request("GET").await }))
        .route("/greetme", post(greet_cust)
                          .get(|| async { wrong_request("POST").await }))
        .route("/announcementsJson", get(announcements_handler_json)
                                .post(|| async { wrong_request("GET").await }))
        .route("/announcementsHtml", get(announcements_handler_html)
                                .post(|| async { wrong_request("GET").await }))
        .fallback(handler_404);

    println!("Running on http://localhost:8080");
    axum::Server::bind(&"0.0.0.0:8080".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}