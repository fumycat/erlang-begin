defmodule Xsip.MixProject do
  use Mix.Project

  def project do
    [
      app: :xsip,
      version: "0.1.0",
      elixir: "~> 1.12-dev",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      env: [
        host: "192.168.0.104",
        remote_host: "192.168.0.104",
        port: 15061,
        remote_port: 5060,
        username: "1001",
        secret: "1234"
      ],
      mod: {XSip.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:sippet, "~> 1.0.9"},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
