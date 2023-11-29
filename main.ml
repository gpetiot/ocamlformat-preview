open Js_of_ocaml
module Html = Dom_html

module Input = struct
  let name = "<user input>"

  let from : Ocamlformat_lib.Conf_t.updated_from =
    `Parsed (`File (Ocaml_common.Location.in_file name))
end

let format source conf =
  let open Ocamlformat_lib in
  match
    Translation_unit.parse_and_format Syntax.Use_file conf
      ~input_name:Input.name ~source
  with
  | Ok formatted -> Ok formatted
  | Error e ->
      let error_buf = Buffer.create 100 in
      let fmt = Format.formatter_of_buffer error_buf in
      Translation_unit.Error.print fmt ~debug:conf.opr_opts.debug.v
        ~quiet:conf.opr_opts.quiet.v e;
      Format.pp_print_flush fmt ();
      let error_msg = Buffer.contents error_buf in
      Error error_msg

let get_element_exn id coerce =
  match Html.getElementById_coerce id coerce with
  | None -> failwith (Printf.sprintf "unable to find element with id %s" id)
  | Some e -> e

let add_option d elt Ocamlformat_lib.Conf_decl.UI.{ names; values; update; doc }
    =
  let name = List.last_exn names in
  let div = Html.createDiv d in
  Dom.appendChild elt div;
  div##.classList##add (Js.string "option");
  let l = Html.createLabel d in
  l##.innerText := Js.string name;
  let dfn = Html.createDfn d in
  dfn##setAttribute (Js.string "data-info") (Js.string doc);
  Dom.appendChild div dfn;
  Dom.appendChild dfn l;
  let kind =
    match values with
    | Bool -> `Choice [ ""; "true"; "false" ]
    | Int -> `Int
    | Range -> `Range
    | Choice v -> `Choice ("" :: v)
    | Ocaml_version ->
        `Choice (List.map Ocaml_version.Releases.all ~f:Ocaml_version.to_string)
  in
  match kind with
  | `Int -> (
      let input =
        Html.createInput ~_type:(Js.string "number") ~name:(Js.string name) d
      in
      Dom.appendChild div input;
      fun conf ->
        match Js.to_string input##.value with
        | "" -> conf
        | s -> update conf s Input.from)
  | `Range -> (
      let lower =
        Html.createInput ~_type:(Js.string "number") ~name:(Js.string name) d
      in
      let upper =
        Html.createInput ~_type:(Js.string "number") ~name:(Js.string name) d
      in
      lower##.classList##add (Js.string "range-lower");
      upper##.classList##add (Js.string "range-upper");
      Dom.appendChild div lower;
      Dom.appendChild div upper;
      fun conf ->
        match (Js.to_string lower##.value, Js.to_string upper##.value) with
        | "", _ | _, "" -> conf
        | l, u -> update conf (l ^ "-" ^ u) Input.from)
  | `Choice values ->
      let s = Html.createSelect ~name:(Js.string name) d in
      Dom.appendChild div s;
      let () =
        List.iter values ~f:(fun v ->
            let o = Html.createOption d in
            o##.label := Js.string v;
            s##add o Js.null)
      in
      fun conf ->
        let i = s##.selectedIndex in
        let v = List.nth_exn values i in
        update conf v Input.from

let format_action doc options _event =
  let editor_doc_get_value () : Js.js_string Js.t =
    Js.Unsafe.fun_call Js.Unsafe.global##.editor##.doc##.getValue [||]
  in
  let editor_doc_set_value (s : Js.js_string Js.t) : unit =
    Js.Unsafe.fun_call
      Js.Unsafe.global##.editor##.doc##.setValue
      [| Js.Unsafe.inject s |]
  in
  let init = Ocamlformat_lib.Conf.default in
  let config = List.fold options ~init ~f:(fun c update -> update c) in
  let log = get_element_exn "log" Html.CoerceTo.div in
  let log kind s =
    let class_ = match kind with `Ok -> "ok" | `Error -> "error" in
    let last_log = Html.createP doc in
    last_log##.textContent := Js.some (Js.string s);
    last_log##.classList##add (Js.string class_);
    Dom.appendChild log last_log
  in
  let () =
    match format (Js.to_string (editor_doc_get_value ())) config with
    | Ok code_formatted ->
        log `Ok "Formatting successful!";
        editor_doc_set_value (Js.string code_formatted)
    | Error err -> log `Error err
  in
  Js._true

let onload _event =
  let () =
    let d = Html.document in
    let profile_div = get_element_exn "profile" Html.CoerceTo.fieldset in
    let fmt_options_div =
      get_element_exn "fmt-options" Html.CoerceTo.fieldset
    in
    let opr_options_div =
      get_element_exn "opr-options" Html.CoerceTo.fieldset
    in
    let format_buttons =
      Dom.list_of_nodeList (d##getElementsByClassName (Js.string "format"))
    in
    let profile = add_option d profile_div Ocamlformat_lib.Conf.UI.profile in
    let fmt_options =
      List.map Ocamlformat_lib.Conf.UI.fmt_opts
        ~f:(add_option d fmt_options_div)
    in
    let opr_options =
      List.map Ocamlformat_lib.Conf.UI.opr_opts
        ~f:(add_option d opr_options_div)
    in
    let options = (profile :: fmt_options) @ opr_options in
    List.iter format_buttons ~f:(fun button ->
        button##.onclick := Html.handler (format_action d options));
    ()
  in
  Js._false

let () = Html.window##.onload := Html.handler onload
