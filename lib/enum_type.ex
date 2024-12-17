defmodule EnumType do
  @moduledoc """
  Generates Enumerated type modules that can be used as values and matched in
  code. Creates proper types so Dialyzer will be able to check bad calls.
  """

  defmacro __using__(_opts) do
    quote do
      import EnumType
    end
  end

  defmacro defenum(name, do: block) do
    quote generated: true, location: :keep do
      defenum(unquote(name), :string, do: unquote(block))
    end
  end

  # Makes an alias: `build_module(MyModule, [:One])` -> `MyModule.One`
  defp build_module({:__aliases__, meta, prefix}, submodules),
    do: {:__aliases__, meta, prefix ++ submodules}

  defp build_module(prefix, submodules) when is_atom(prefix),
    do: {:__aliases__, [], [prefix | submodules]}

  # Given a prefix like `:MyModule` and some subtypes like `[:One,
  # :Two]`, constructs a pipe list like `MyModule.One | MyModule.Two`.
  # Returns AST.
  defp build_type_pipe(prefix, [subtype]) do
    build_module(prefix, [subtype])
  end

  defp build_type_pipe(prefix, [subtype | other_types]) do
    quote generated: true, location: :keep do
      unquote(build_module(prefix, [subtype])) | unquote(build_type_pipe(prefix, other_types))
    end
  end

  # Extracts the value definitions from the defenum block
  # A do block with only one expression is not wrapped in a {:__block__, ..} AST node
  defp extract_values({:__block__, _, block_body}, acc),
    do: Enum.reduce(block_body, acc, &extract_values/2)

  defp extract_values({:value, _, [{_, _, [sym]} | _]}, acc), do: [sym | acc]
  defp extract_values(_, acc), do: acc

  defmacro defenum(name, ecto_type, do: block) do
    values = extract_values(block, [])
    type_pipe = build_type_pipe(name, values)

    syn =
      quote generated: true, location: :keep do
        defmodule unquote(name) do
          @type t :: unquote(type_pipe)

          Module.register_attribute(__MODULE__, :possible_options, accumulate: true)

          with {:module, _module} <- Code.ensure_compiled(Ecto.Type) do
            @behaviour Ecto.Type
            def embed_as(_), do: :dump
            def equal?(term1, term2), do: term1 == term2

            def type, do: unquote(ecto_type)
          end

          def default, do: nil

          defoverridable default: 0

          unquote(block)

          with {:module, _module} <- Code.ensure_compiled(Ecto.Type) do
            # Default fallback ecto conversion options.
            def cast(_), do: :error
            def load(_), do: :error
            def value(nil), do: nil
            def value(_), do: :error
            def dump(_), do: :error

            def validate(changeset, field, opts \\ []) do
              Ecto.Changeset.validate_inclusion(changeset, field, enums(), opts)
            end
          end

          def enums, do: Enum.reverse(Enum.map(@possible_options, fn {key, _value} -> key end))
          def values, do: Enum.reverse(Enum.map(@possible_options, fn {_key, value} -> value end))
          def options, do: Enum.reverse(@possible_options)
        end
      end

    syn
  end

  defmacro default(option) do
    quote generated: true, location: :keep do
      def default, do: __MODULE__.unquote(option)
    end
  end

  defmacro value(option, value, [do: block] \\ [do: nil]) do
    quote generated: true, location: :keep do
      @possible_options {__MODULE__.unquote(option), unquote(value)}

      defmodule unquote(option) do
        @type t :: __MODULE__

        def value, do: unquote(value)
        def upcase_value, do: String.upcase(value() |> to_string())
        def downcase_value, do: String.downcase(value() |> to_string())
        unquote(block)
      end

      def from(unquote(value)), do: unquote(option)

      def value(unquote(option)), do: unquote(option).value

      with {:module, _module} <- Code.ensure_compiled(Ecto.Type) do
        # Support querying by both the Enum module and the specific value.
        # Error will occur if an invalid value is attempted to be used.
        def cast(unquote(option)), do: {:ok, unquote(option)}
        def cast(unquote(value)), do: {:ok, unquote(option)}

        def load(unquote(value)), do: {:ok, unquote(option)}

        # Allow both querying by Module and setting a value to the Module when updating or inserting.
        def dump(unquote(option)), do: {:ok, unquote(option).value}
      end
    end
  end
end
