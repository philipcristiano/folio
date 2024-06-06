use std::fs;

pub fn read_app_config(path: String) -> crate::AppConfig {
    let config_file_error_msg = format!("Could not read config file {}", path);
    let config_file_contents = fs::read_to_string(path).expect(&config_file_error_msg);
    let app_config: crate::AppConfig =
        toml::from_str(&config_file_contents).expect("Problems parsing config file");

    app_config
}
