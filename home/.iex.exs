import_if_available(Ecto.Query)
import_if_available(Ecto.Changeset)

IEx.configure(
  colors: [
    syntax_colors: [
      number: :light_yellow,
      atom: :light_cyan,
      string: :light_black,
      boolean: [:light_blue],
      nil: [:magenta, :bright]
    ],
    ls_directory: :cyan,
    ls_device: :yellow,
    doc_code: :green,
    doc_inline_code: :magenta,
    doc_headings: [:cyan, :underline],
    doc_title: [:cyan, :bright, :underline]
  ],
  inspect: [
    pretty: true,
    limit: :infinity,
    charlists: :as_lists,
    custom_options: [sort_maps: true]
  ]
)

if :mix not in Enum.map(Application.started_applications(), &elem(&1, 0)) do
  Mix.install([
    :decimal,
    :jason
  ])
end

defmodule PB do
  def copy(term) do
    text =
      if is_binary(term) do
        term
      else
        inspect(term, limit: :infinity, pretty: true)
      end

    port = Port.open({:spawn, "pbcopy"}, [])
    true = Port.command(port, text)
    true = Port.close(port)

    term
  end
end
