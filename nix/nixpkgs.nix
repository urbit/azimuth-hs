{ ... }@args:

let

  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/7e07846d99bed4bd7272b1626038e77a78aa36f6.tar.gz";
    sha256 = "1dx82cj4pyjv1fdxbfqp0j7bpm869hyjyvnz57zi9pbp20awjzjr";
  };

in

  import nixpkgsSrc args

