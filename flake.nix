{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };
  outputs = {nixpkgs} @inputs: {
    packages = { hello = nixpkgs.hello; };
  };
}
