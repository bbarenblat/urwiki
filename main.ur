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
                 Title : string,
                 Content : string }
  PRIMARY KEY Id
sequence commit_id_next

datatype mode = View | Edit
val eq_mode =
  mkEq (fn x y =>
          case (x, y) of
              (View, View) => True
            | (Edit, Edit) => True
            | _ => False)

fun only_in mode_source target_mode =
  current_mode <- signal mode_source;
  return (if current_mode = target_mode
          then null
          else Style.invisible)

fun current_revision title =
  commits <- queryL1 (SELECT commit.Created, commit.Title, commit.Content
                      FROM commit
                      WHERE commit.Title = {[title]}
                      ORDER BY commit.Created ASC);
  return (case commits of
              Nil => {Title = title, Body = "Not found."}
            | art :: _ => {Title = art.Title,
                           Body = show art.Content})

fun create_commit title text : transaction unit =
  id <- nextval commit_id_next;
  creation_time <- now;
  dml (INSERT INTO commit (Id, Created, Title, Content)
       VALUES ({[id]}, {[creation_time]}, {[title]}, {[text]}))

fun wiki requested_article_title =
  (* Look up the article. *)
  article <- current_revision requested_article_title;
  (* Stuff the article text in a source so we can live-update it as the user
  edits. *)
  article_body_source <- source article.Body;
  (* Initially, we're in View mode, and we can switch to Edit mode on user
  request. *)
  page_mode <- source View;
  return
    <xml>
      <head>
        <title>
          {[if article.Title = Configuration.main_page_name
            then Configuration.wiki_title
            else article.Title ^ " – " ^ Configuration.wiki_title]}
        </title>
        <link rel="stylesheet" href="/urwiki.css" />
      </head>
      <body>
        (* Page headings *)
        <div>
          <h1>{[Configuration.wiki_title]}</h1>
          <ul>
            <li>
              <a link={wiki Configuration.main_page_name}>
                {[Configuration.main_page_name]}
              </a>
            </li>
          </ul>
        </div>
        (* Article *)
        <dyn signal={text <- signal article_body_source;
                     return <xml>{[text]}</xml>} /><br />
        (* Editing panel *)
        <div>
          (* Controls for View mode *)
          <div dynClass={only_in page_mode View}>
            <button
               value="Edit"
               onclick={fn _ =>
                          lock_result <- rpc (Lock.take article.Title);
                          if lock_result
                            then set page_mode Edit
                            else return ()} />
          </div>
          (* Controls for Edit mode *)
          <div dynClass={only_in page_mode Edit}>
            <ctextarea source={article_body_source} /><br />
            <button
               value="Commit"
               onclick={fn _ =>
                          text <- get article_body_source;
                          rpc (create_commit article.Title text);
                          rpc (Lock.release article.Title);
                          set page_mode View} />
          </div>
        </div>
      </body>
    </xml>
