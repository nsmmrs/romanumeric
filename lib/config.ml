let home =
  let env_var =
    match Sys.os_type with
    | "Unix" -> "HOME"
    | _ -> "APPDATA"
  in
  Sys.getenv_opt env_var
  |> Option.to_result ~none:(Romanum_error.missing_env env_var)

let default_cache_dir =
  Result.map
    (fun home -> "romanum" |> Filename.concat ".cache" |> Filename.concat home)
    home

let default_config_dir =
  Result.map
    (fun home -> "romanum" |> Filename.concat ".config" |> Filename.concat home)
    home

let cache_dir =
  Sys.getenv_opt "ROMANUM_CACHE_DIR"
  |> Option.map Result.ok
  |> Option.value ~default:default_cache_dir

let config_dir =
  Sys.getenv_opt "ROMANUM_CONFIG_DIR"
  |> Option.map Result.ok
  |> Option.value ~default:default_config_dir
