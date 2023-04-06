-module(otpbp_shell_docs).

-ifdef(HAVE_shell_docs__module_info_0).
-ifndef(HAVE_shell_docs__render_5).
% OTP 23.2
-export([render/5]).
-endif.
-ifndef(HAVE_shell_docs__render_callback_5).
% OTP 23.2
-export([render_callback/5]).
-endif.

-ifndef(HAVE_shell_docs_supported_tags_0).
% OTP 24.0
-export([supported_tags/0]).
-endif.

-ifndef(HAVE_shell_docs__supported_tags_0).
-define(ALL_ELEMENTS, [a, p, 'div', br, h1, h2, h3, i, em, pre, code, ul, ol, li, dl, dt, dd]).
supported_tags() -> ?ALL_ELEMENTS.
-endif.

-ifndef(HAVE_shell_docs__render_5).
render(Module, Function, Arity, Docs, _Config) -> shell_docs:render(Module, Function, Arity, Docs).
-endif.

-ifndef(HAVE_shell_docs__render_5).
render_callback(Module, Callback, Arity, Docs, _Config) -> shell_docs:render_callback(Module, Callback, Arity, Docs).
-endif.
-endif.
