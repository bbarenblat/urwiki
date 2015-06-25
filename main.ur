(* Copyright 2015 the Massachusetts Institute of Technology

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License.  You may obtain a copy of the
License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied.  See the License for the
specific language governing permissions and limitations under the License. *)

structure Configuration = struct
  val main_page_name = "Main Page"
  val wiki_title = "UrWiki demo"
end

type id = int

table commit : { Id : id,
                 Created : time,
                 Content : string }
  PRIMARY KEY Id
sequence commit_id_next

table article : { Title : string,
                  Head : id }
  PRIMARY KEY Title,
  CONSTRAINT Head
    FOREIGN KEY Head REFERENCES commit(Id)

fun commit_article title text : transaction unit =
  id <- nextval commit_id_next;
  creation_time <- now;
  dml (INSERT INTO commit (Id, Created, Content)
       VALUES ({[id]}, {[creation_time]}, {[text]}));
  (* TODO(bbaren): This is ugly.  Use CAS instead? *)
  sql_error <- tryDml (INSERT INTO article (Title, Head)
                       VALUES ({[title]}, {[id]}));
  case sql_error of
      None =>
        (* We created a new article. *)
        return ()
    | Some _ =>
        (* The article already exists. *)
        dml (UPDATE article
             SET Head = {[id]}
             WHERE Title = {[title]})

fun wiki requested_article_title =
  extant_articles <-
    queryL (SELECT article.Title, commit.Content
             FROM article LEFT JOIN commit ON article.Head = commit.Id
             WHERE article.Title = {[requested_article_title]});
  let
    val article =
      case extant_articles of
          Nil => {Title = requested_article_title, Body = "Not found."}
        | art :: Nil => {Title = art.Article.Title,
                         Body = show art.Commit.Content}
        | _ :: _ :: _ => error
                           <xml>
                             Multiple articles with title
                             ‘{[requested_article_title]}’
                           </xml>
  in
    article_body_source <- source article.Body;
    return
      <xml>
        <head>
          <title>
            {[if article.Title = Configuration.main_page_name
              then Configuration.wiki_title
              else article.Title ^ " – " ^ Configuration.wiki_title]}
          </title>
        </head>
        <body>
          <h1>{[Configuration.wiki_title]}</h1>
          <ul>
            <li>
              <a link={wiki Configuration.main_page_name}>
                {[Configuration.main_page_name]}
              </a>
            </li>
          </ul>
          <dyn signal={text <- signal article_body_source;
                       return <xml>{[text]}</xml>} /><br />
          <ctextarea source={article_body_source} /><br />
          <button
             value="Commit"
             onclick={fn _ =>
                        text <- get article_body_source;
                        rpc (commit_article article.Title text)} />
        </body>
      </xml>
  end
