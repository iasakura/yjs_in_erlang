-module(type_ref).

-export([decode_type_ref/1, encode_type_ref/1]).
-export_type([type_ref/0]).

-include("../include/constants.hrl").

% 下記のrust型をunion型で表現
% #[repr(u8)]
% #[derive(Debug, Clone, Eq, PartialEq)]
% pub enum TypeRef {
%     Array = TYPE_REFS_ARRAY,
%     Map = TYPE_REFS_MAP,
%     Text = TYPE_REFS_TEXT,
%     XmlElement(Arc<str>) = TYPE_REFS_XML_ELEMENT,
%     XmlFragment = TYPE_REFS_XML_FRAGMENT,
%     XmlHook = TYPE_REFS_XML_HOOK,
%     XmlText = TYPE_REFS_XML_TEXT,
%     SubDoc = TYPE_REFS_DOC,
%     #[cfg(feature = "weak")]
%     WeakLink(Arc<LinkSource>) = TYPE_REFS_WEAK,
%     Undefined = TYPE_REFS_UNDEFINED,
% }
-type type_ref() ::
    {array}
    | {map}
    | {text}
    | {xml_element, binary()}
    | {xml_fragment}
    | {xml_hook}
    | {xml_text}
    | {sub_doc}
    % | {weak_link, link_source()}
    | {undefined}.

-spec decode_type_ref(binary()) -> {type_ref(), binary()}.
decode_type_ref(Bin) ->
    case var_int:decode_uint(Bin) of
        {?TYPE_REFS_ARRAY, Rest} ->
            {{array}, Rest};
        {?TYPE_REFS_MAP, Rest} ->
            {{map}, Rest};
        {?TYPE_REFS_TEXT, Rest} ->
            {{text}, Rest};
        {?TYPE_REFS_XML_ELEMENT, Rest} ->
            {Buf, Rest1} = binary_encoding:decode_string(Rest),
            {{xml_element, Buf}, Rest1};
        {?TYPE_REFS_XML_FRAGMENT, Rest} ->
            {{xml_fragment}, Rest};
        {?TYPE_REFS_XML_HOOK, Rest} ->
            {{xml_hook}, Rest};
        {?TYPE_REFS_XML_TEXT, Rest} ->
            {{xml_text}, Rest};
        {?TYPE_REFS_DOC, Rest} ->
            {{sub_doc}, Rest};
        {?TYPE_REFS_UNDEFINED, Rest} ->
            {{undefined}, Rest}
    end.

-spec encode_type_ref(type_ref()) -> binary().
encode_type_ref({array}) ->
    <<(var_int:encode_uint(?TYPE_REFS_ARRAY))/binary>>;
encode_type_ref({map}) ->
    <<(var_int:encode_uint(?TYPE_REFS_MAP))/binary>>;
encode_type_ref({text}) ->
    <<(var_int:encode_uint(?TYPE_REFS_TEXT))/binary>>;
encode_type_ref({xml_element, Buf}) ->
    <<
        (var_int:encode_uint(?TYPE_REFS_XML_ELEMENT))/binary,
        (binary_encoding:encode_string(Buf))/binary
    >>;
encode_type_ref({xml_fragment}) ->
    <<(var_int:encode_uint(?TYPE_REFS_XML_FRAGMENT))/binary>>;
encode_type_ref({xml_hook}) ->
    <<(var_int:encode_uint(?TYPE_REFS_XML_HOOK))/binary>>;
encode_type_ref({xml_text}) ->
    <<(var_int:encode_uint(?TYPE_REFS_XML_TEXT))/binary>>;
encode_type_ref({sub_doc}) ->
    <<(var_int:encode_uint(?TYPE_REFS_DOC))/binary>>;
encode_type_ref({undefined}) ->
    <<(var_int:encode_uint(?TYPE_REFS_UNDEFINED))/binary>>.
