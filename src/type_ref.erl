-module(type_ref).

-export_type([type_ref/0]).

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
