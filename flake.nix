{
  inputs = {
    nixpkgs.url = "github:nixos/nixpgs";
  };
  outputs = {nixpkgs} @inputs: {
    packages = { hello = nixpkgs.hello; };
  };
}
