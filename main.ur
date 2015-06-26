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
              Nil => {Title = title, Body = "Not found.", Modified = None}
            | art :: _ => {Title = art.Title,
                           Body = show art.Content,
                           Modified = Some art.Created})

fun create_commit title text : transaction unit =
  id <- nextval commit_id_next;
  creation_time <- now;
  dml (INSERT INTO commit (Id, Created, Title, Content)
       VALUES ({[id]}, {[creation_time]}, {[title]}, {[text]}))

fun report_last_modified date =
  <xml>
    This page was last modified at
    {[timef "%l:%M:%S %P %Z on %A, %e %B %Y" date]}.
  </xml>

style content
style footer
style button_group
style top_bar

fun wiki requested_article_title =
  (* Look up the article. *)
  article <- current_revision requested_article_title;
  (* Stuff article information into sources so we can live-update them as the
  user edits. *)
  article_body_source <- source article.Body;
  article_modified_source <- source article.Modified;
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
        <link rel="stylesheet" href="/css/normalize.css" />
        <link rel="stylesheet" href="/css/urwiki.css" />
      </head>
      <body>
        (* Page headings *)
        <div class={top_bar}>
          <a link={wiki Configuration.main_page_name}>
            {[Configuration.wiki_title]}
          </a>
          <div class={button_group}>
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
              <button
                 value="Commit"
                 onclick={fn _ =>
                            text <- get article_body_source;
                            rpc (create_commit article.Title text);
                            rpc (Lock.release article.Title);
                            set page_mode View} />
              <button
                 value="Revert"
                 onclick={fn _ =>
                            set article_body_source article.Body;
                            rpc (Lock.release article.Title);
                            set page_mode View} />
            </div>
          </div>
        </div>
        (* Article *)
        <div class={content}>
          (* Editing window *)
          <div dynClass={only_in page_mode Edit}>
            <ctextarea source={article_body_source} /><br />
          </div>
          (* Article text (or live preview) *)
          <dyn signal={text <- signal article_body_source;
                       return <xml>{[text]}</xml>} />
          <div class={footer}>
            <dyn signal={modified <- signal article_modified_source;
                         return (case article.Modified of
                                     None => <xml></xml>
                                   | Some date => report_last_modified date)} />
          </div>
        </div>
      </body>
    </xml>
