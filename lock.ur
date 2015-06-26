(* Copyright 2015 the Massachusetts Institute of Technology

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License.  You may obtain a copy of the
License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied.  See the License for the
specific language governing permissions and limitations under the License. *)

table lock : { Title : string,
               Holder : client }
  PRIMARY KEY Title

fun take page_title =
  new_holder <- self;
  result <- tryDml (INSERT INTO lock (Title, Holder)
                    VALUES ({[page_title]}, {[new_holder]}));
  return (Option.isNone result)

fun release page_title =
  requestor <- self;
  dml (DELETE FROM lock
       WHERE Holder = {[requestor]})
