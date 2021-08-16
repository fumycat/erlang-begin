defmodule DbConfig do
  import SweetXml

  def read_config do
    {:ok, xmldoc} = File.read(Path.expand("config.xml"))

    enable_sql = xmldoc |> xpath(~x"/*/mySql/@enabled") |> to_string |> String.to_existing_atom
    enable_mnesia = xmldoc |> xpath(~x"/*/mnesia/@enabled") |> to_string |> String.to_existing_atom

    mysql_hostname = xmldoc |> xpath(~x"/settings/mySql/host/text()") |> to_string
    mysql_username = xmldoc |> xpath(~x"/settings/mySql/username/text()") |> to_string
    mysql_password = xmldoc |> xpath(~x"/settings/mySql/password/text()") |> to_string
    mysql_db_name = xmldoc |> xpath(~x"/settings/mySql/databaseName/text()") |> to_string

    # IO.inspect {{enable_sql, mysql_hostname, mysql_username, mysql_password, mysql_db_name}, {enable_mnesia}}

    {{enable_sql, mysql_hostname, mysql_username, mysql_password, mysql_db_name}, {enable_mnesia}}
  end

end
