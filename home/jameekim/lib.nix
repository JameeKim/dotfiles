# Additions to `config.lib`.
{
  flake,
  config,
  lib,
  ...
}:
{
  config.lib.file = {
    # Absolute path to this dotfiles git repository.
    dotfilesPath = "${config.home.homeDirectory}/dev/dotfiles";
    # Creates a symlink to a path inside this dotfiles repository.
    # Unlike `mkOutOfStoreSymlink`, this links to the source path, not store.
    # Expects a path inside the store of this repository.
    mkDotfilesSymlink =
      path:
      let
        pathStr = toString path;
        storePathStr = toString flake;
        fullPathStr = config.lib.file.dotfilesPath + lib.removePrefix storePathStr pathStr;
      in
      if lib.hasPrefix (storePathStr + "/") pathStr then
        config.lib.file.mkOutOfStoreSymlink fullPathStr
      else
        throw "expected a Path in ${storePathStr} but found: ${pathStr}";
  };
}
