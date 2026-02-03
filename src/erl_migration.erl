-module(erl_migration).

-callback init(Args :: list(term())) -> ok.

-callback get_current_rev() -> any().

-callback get_prev_rev() -> any().

-callback up() -> any().

-callback down() -> any().
