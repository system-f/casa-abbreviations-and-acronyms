{ nixpkgs, declInput }: let pkgs = import nixpkgs {}; in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
        "casa-abbreviations-and-acronyms": {
            "enabled": 1,
            "hidden": false,
            "description": "casa-abbreviations-and-acronyms",
            "nixexprinput": "casa-abbreviations-and-acronyms",
            "nixexprpath": "ci/ci.nix",
            "checkinterval": 300,
            "schedulingshares": 1,
            "enableemail": false,
            "emailoverride": "",
            "keepnr": 5,
            "inputs": {
                "casa-abbreviations-and-acronyms": { "type": "git", "value": "https://github.com/qfpl/casa-abbreviations-and-acronyms", "emailresponsible": false },
                "nixpkgs": { "type": "git", "value": "https://github.com/NixOS/nixpkgs.git release-17.09", "emailresponsible": false }
            }
        }
    }
    EOF
  '';
}
