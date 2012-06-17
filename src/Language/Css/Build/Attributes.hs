-- | Html 4 ++ Html 5 attributes
module Language.Css.Build.Attributes (
abbr,
accept,
acceptCharset,
accesskey,
action,
align,
alt,
archive,
async,
autocomplete,
autofocus,
autoplay,
axis,
background,
bgcolor,
border,
cellpadding,
cellspacing,
challenge,
char,
charoff,
charset,
checked,
cite,
class',
classid,
clear,
codebase,
codetype,
cols,
colspan,
compact,
content,
contenteditable,
contextmenu,
controls,
coords,
data',
datetime,
declare,
defer,
dir,
disabled,
draggable,
enctype,
for,
form,
formaction,
formenctype,
formmethod,
formnovalidate,
formtarget,
frame,
frameborder,
headers,
height,
hidden,
high,
href,
hreflang,
hspace,
httpEquiv,
icon,
id',
ismap,
item,
itemprop,
keytype,
label,
lang,
language,
list,
loop,
low,
manifest,
max,
maxlength,
media,
method,
min,
multiple,
name,
nohref,
noshade,
novalidate,
nowrap,
onabort,
onbeforeonload,
onbeforeprint,
onblur,
oncanplay,
oncanplaythrough,
onchange,
onclick,
oncontextmenu,
ondblclick,
ondrag,
ondragend,
ondragenter,
ondragleave,
ondragover,
ondragstart,
ondrop,
ondurationchange,
onemptied,
onended,
onerror,
onfocus,
onformchange,
onforminput,
onhaschange,
oninput,
oninvalid,
onkeydown,
onkeypress,
onkeyup,
onload,
onloadeddata,
onloadedmetadata,
onloadstart,
onmessage,
onmousedown,
onmousemove,
onmouseout,
onmouseover,
onmouseup,
onmousewheel,
ononline,
onpagehide,
onpageshow,
onpause,
onplay,
onplaying,
onprogress,
onpropstate,
onratechange,
onreadystatechange,
onredo,
onreset,
onresize,
onscroll,
onseeked,
onseeking,
onselect,
onstalled,
onstorage,
onsubmit,
onsuspend,
ontimeupdate,
onundo,
onunload,
onvolumechange,
onwaiting,
open,
optimum,
pattern,
ping,
placeholder,
preload,
profile,
pubdate,
radiogroup,
readonly,
rel,
required,
rev,
reversed,
rows,
rowspan,
rules,
sandbox,
scheme,
scope,
scoped,
scrolling,
seamless,
selected,
shape,
size,
sizes,
span,
spellcheck,
src,
srcdoc,
standby,
start,
step,
style,
subject,
summary,
tabindex,
target,
title,
type',
usemap,
valign,
value,
valuetype,
vspace,
width,
wrap,
xmlns
) where
import Language.Css.Syntax(Attr)
import Language.Css.Build(Idents(..))
import Prelude ()


-- | @abbr@ attribute
abbr :: Attr
abbr = ident "abbr"


-- | @accept@ attribute
accept :: Attr
accept = ident "accept"


-- | @acceptCharset@ attribute
acceptCharset :: Attr
acceptCharset = ident "acceptCharset"


-- | @accesskey@ attribute
accesskey :: Attr
accesskey = ident "accesskey"


-- | @action@ attribute
action :: Attr
action = ident "action"


-- | @align@ attribute
align :: Attr
align = ident "align"


-- | @alt@ attribute
alt :: Attr
alt = ident "alt"


-- | @archive@ attribute
archive :: Attr
archive = ident "archive"


-- | @async@ attribute
async :: Attr
async = ident "async"


-- | @autocomplete@ attribute
autocomplete :: Attr
autocomplete = ident "autocomplete"


-- | @autofocus@ attribute
autofocus :: Attr
autofocus = ident "autofocus"


-- | @autoplay@ attribute
autoplay :: Attr
autoplay = ident "autoplay"


-- | @axis@ attribute
axis :: Attr
axis = ident "axis"


-- | @background@ attribute
background :: Attr
background = ident "background"


-- | @bgcolor@ attribute
bgcolor :: Attr
bgcolor = ident "bgcolor"


-- | @border@ attribute
border :: Attr
border = ident "border"


-- | @cellpadding@ attribute
cellpadding :: Attr
cellpadding = ident "cellpadding"


-- | @cellspacing@ attribute
cellspacing :: Attr
cellspacing = ident "cellspacing"


-- | @challenge@ attribute
challenge :: Attr
challenge = ident "challenge"


-- | @char@ attribute
char :: Attr
char = ident "char"


-- | @charoff@ attribute
charoff :: Attr
charoff = ident "charoff"


-- | @charset@ attribute
charset :: Attr
charset = ident "charset"


-- | @checked@ attribute
checked :: Attr
checked = ident "checked"


-- | @cite@ attribute
cite :: Attr
cite = ident "cite"


-- | @class'@ attribute
class' :: Attr
class' = ident "class"


-- | @classid@ attribute
classid :: Attr
classid = ident "classid"


-- | @clear@ attribute
clear :: Attr
clear = ident "clear"


-- | @codebase@ attribute
codebase :: Attr
codebase = ident "codebase"


-- | @codetype@ attribute
codetype :: Attr
codetype = ident "codetype"


-- | @cols@ attribute
cols :: Attr
cols = ident "cols"


-- | @colspan@ attribute
colspan :: Attr
colspan = ident "colspan"


-- | @compact@ attribute
compact :: Attr
compact = ident "compact"


-- | @content@ attribute
content :: Attr
content = ident "content"


-- | @contenteditable@ attribute
contenteditable :: Attr
contenteditable = ident "contenteditable"


-- | @contextmenu@ attribute
contextmenu :: Attr
contextmenu = ident "contextmenu"


-- | @controls@ attribute
controls :: Attr
controls = ident "controls"


-- | @coords@ attribute
coords :: Attr
coords = ident "coords"


-- | @data'@ attribute
data' :: Attr
data' = ident "data"


-- | @datetime@ attribute
datetime :: Attr
datetime = ident "datetime"


-- | @declare@ attribute
declare :: Attr
declare = ident "declare"


-- | @defer@ attribute
defer :: Attr
defer = ident "defer"


-- | @dir@ attribute
dir :: Attr
dir = ident "dir"


-- | @disabled@ attribute
disabled :: Attr
disabled = ident "disabled"


-- | @draggable@ attribute
draggable :: Attr
draggable = ident "draggable"


-- | @enctype@ attribute
enctype :: Attr
enctype = ident "enctype"


-- | @for@ attribute
for :: Attr
for = ident "for"


-- | @form@ attribute
form :: Attr
form = ident "form"


-- | @formaction@ attribute
formaction :: Attr
formaction = ident "formaction"


-- | @formenctype@ attribute
formenctype :: Attr
formenctype = ident "formenctype"


-- | @formmethod@ attribute
formmethod :: Attr
formmethod = ident "formmethod"


-- | @formnovalidate@ attribute
formnovalidate :: Attr
formnovalidate = ident "formnovalidate"


-- | @formtarget@ attribute
formtarget :: Attr
formtarget = ident "formtarget"


-- | @frame@ attribute
frame :: Attr
frame = ident "frame"


-- | @frameborder@ attribute
frameborder :: Attr
frameborder = ident "frameborder"


-- | @headers@ attribute
headers :: Attr
headers = ident "headers"


-- | @height@ attribute
height :: Attr
height = ident "height"


-- | @hidden@ attribute
hidden :: Attr
hidden = ident "hidden"


-- | @high@ attribute
high :: Attr
high = ident "high"


-- | @href@ attribute
href :: Attr
href = ident "href"


-- | @hreflang@ attribute
hreflang :: Attr
hreflang = ident "hreflang"


-- | @hspace@ attribute
hspace :: Attr
hspace = ident "hspace"


-- | @httpEquiv@ attribute
httpEquiv :: Attr
httpEquiv = ident "httpEquiv"


-- | @icon@ attribute
icon :: Attr
icon = ident "icon"


-- | @id'@ attribute
id' :: Attr
id' = ident "id"


-- | @ismap@ attribute
ismap :: Attr
ismap = ident "ismap"


-- | @item@ attribute
item :: Attr
item = ident "item"


-- | @itemprop@ attribute
itemprop :: Attr
itemprop = ident "itemprop"


-- | @keytype@ attribute
keytype :: Attr
keytype = ident "keytype"


-- | @label@ attribute
label :: Attr
label = ident "label"


-- | @lang@ attribute
lang :: Attr
lang = ident "lang"


-- | @language@ attribute
language :: Attr
language = ident "language"


-- | @list@ attribute
list :: Attr
list = ident "list"


-- | @loop@ attribute
loop :: Attr
loop = ident "loop"


-- | @low@ attribute
low :: Attr
low = ident "low"


-- | @manifest@ attribute
manifest :: Attr
manifest = ident "manifest"


-- | @max@ attribute
max :: Attr
max = ident "max"


-- | @maxlength@ attribute
maxlength :: Attr
maxlength = ident "maxlength"


-- | @media@ attribute
media :: Attr
media = ident "media"


-- | @method@ attribute
method :: Attr
method = ident "method"


-- | @min@ attribute
min :: Attr
min = ident "min"


-- | @multiple@ attribute
multiple :: Attr
multiple = ident "multiple"


-- | @name@ attribute
name :: Attr
name = ident "name"


-- | @nohref@ attribute
nohref :: Attr
nohref = ident "nohref"


-- | @noshade@ attribute
noshade :: Attr
noshade = ident "noshade"


-- | @novalidate@ attribute
novalidate :: Attr
novalidate = ident "novalidate"


-- | @nowrap@ attribute
nowrap :: Attr
nowrap = ident "nowrap"


-- | @onabort@ attribute
onabort :: Attr
onabort = ident "onabort"


-- | @onbeforeonload@ attribute
onbeforeonload :: Attr
onbeforeonload = ident "onbeforeonload"


-- | @onbeforeprint@ attribute
onbeforeprint :: Attr
onbeforeprint = ident "onbeforeprint"


-- | @onblur@ attribute
onblur :: Attr
onblur = ident "onblur"


-- | @oncanplay@ attribute
oncanplay :: Attr
oncanplay = ident "oncanplay"


-- | @oncanplaythrough@ attribute
oncanplaythrough :: Attr
oncanplaythrough = ident "oncanplaythrough"


-- | @onchange@ attribute
onchange :: Attr
onchange = ident "onchange"


-- | @onclick@ attribute
onclick :: Attr
onclick = ident "onclick"


-- | @oncontextmenu@ attribute
oncontextmenu :: Attr
oncontextmenu = ident "oncontextmenu"


-- | @ondblclick@ attribute
ondblclick :: Attr
ondblclick = ident "ondblclick"


-- | @ondrag@ attribute
ondrag :: Attr
ondrag = ident "ondrag"


-- | @ondragend@ attribute
ondragend :: Attr
ondragend = ident "ondragend"


-- | @ondragenter@ attribute
ondragenter :: Attr
ondragenter = ident "ondragenter"


-- | @ondragleave@ attribute
ondragleave :: Attr
ondragleave = ident "ondragleave"


-- | @ondragover@ attribute
ondragover :: Attr
ondragover = ident "ondragover"


-- | @ondragstart@ attribute
ondragstart :: Attr
ondragstart = ident "ondragstart"


-- | @ondrop@ attribute
ondrop :: Attr
ondrop = ident "ondrop"


-- | @ondurationchange@ attribute
ondurationchange :: Attr
ondurationchange = ident "ondurationchange"


-- | @onemptied@ attribute
onemptied :: Attr
onemptied = ident "onemptied"


-- | @onended@ attribute
onended :: Attr
onended = ident "onended"


-- | @onerror@ attribute
onerror :: Attr
onerror = ident "onerror"


-- | @onfocus@ attribute
onfocus :: Attr
onfocus = ident "onfocus"


-- | @onformchange@ attribute
onformchange :: Attr
onformchange = ident "onformchange"


-- | @onforminput@ attribute
onforminput :: Attr
onforminput = ident "onforminput"


-- | @onhaschange@ attribute
onhaschange :: Attr
onhaschange = ident "onhaschange"


-- | @oninput@ attribute
oninput :: Attr
oninput = ident "oninput"


-- | @oninvalid@ attribute
oninvalid :: Attr
oninvalid = ident "oninvalid"


-- | @onkeydown@ attribute
onkeydown :: Attr
onkeydown = ident "onkeydown"


-- | @onkeypress@ attribute
onkeypress :: Attr
onkeypress = ident "onkeypress"


-- | @onkeyup@ attribute
onkeyup :: Attr
onkeyup = ident "onkeyup"


-- | @onload@ attribute
onload :: Attr
onload = ident "onload"


-- | @onloadeddata@ attribute
onloadeddata :: Attr
onloadeddata = ident "onloadeddata"


-- | @onloadedmetadata@ attribute
onloadedmetadata :: Attr
onloadedmetadata = ident "onloadedmetadata"


-- | @onloadstart@ attribute
onloadstart :: Attr
onloadstart = ident "onloadstart"


-- | @onmessage@ attribute
onmessage :: Attr
onmessage = ident "onmessage"


-- | @onmousedown@ attribute
onmousedown :: Attr
onmousedown = ident "onmousedown"


-- | @onmousemove@ attribute
onmousemove :: Attr
onmousemove = ident "onmousemove"


-- | @onmouseout@ attribute
onmouseout :: Attr
onmouseout = ident "onmouseout"


-- | @onmouseover@ attribute
onmouseover :: Attr
onmouseover = ident "onmouseover"


-- | @onmouseup@ attribute
onmouseup :: Attr
onmouseup = ident "onmouseup"


-- | @onmousewheel@ attribute
onmousewheel :: Attr
onmousewheel = ident "onmousewheel"


-- | @ononline@ attribute
ononline :: Attr
ononline = ident "ononline"


-- | @onpagehide@ attribute
onpagehide :: Attr
onpagehide = ident "onpagehide"


-- | @onpageshow@ attribute
onpageshow :: Attr
onpageshow = ident "onpageshow"


-- | @onpause@ attribute
onpause :: Attr
onpause = ident "onpause"


-- | @onplay@ attribute
onplay :: Attr
onplay = ident "onplay"


-- | @onplaying@ attribute
onplaying :: Attr
onplaying = ident "onplaying"


-- | @onprogress@ attribute
onprogress :: Attr
onprogress = ident "onprogress"


-- | @onpropstate@ attribute
onpropstate :: Attr
onpropstate = ident "onpropstate"


-- | @onratechange@ attribute
onratechange :: Attr
onratechange = ident "onratechange"


-- | @onreadystatechange@ attribute
onreadystatechange :: Attr
onreadystatechange = ident "onreadystatechange"


-- | @onredo@ attribute
onredo :: Attr
onredo = ident "onredo"


-- | @onreset@ attribute
onreset :: Attr
onreset = ident "onreset"


-- | @onresize@ attribute
onresize :: Attr
onresize = ident "onresize"


-- | @onscroll@ attribute
onscroll :: Attr
onscroll = ident "onscroll"


-- | @onseeked@ attribute
onseeked :: Attr
onseeked = ident "onseeked"


-- | @onseeking@ attribute
onseeking :: Attr
onseeking = ident "onseeking"


-- | @onselect@ attribute
onselect :: Attr
onselect = ident "onselect"


-- | @onstalled@ attribute
onstalled :: Attr
onstalled = ident "onstalled"


-- | @onstorage@ attribute
onstorage :: Attr
onstorage = ident "onstorage"


-- | @onsubmit@ attribute
onsubmit :: Attr
onsubmit = ident "onsubmit"


-- | @onsuspend@ attribute
onsuspend :: Attr
onsuspend = ident "onsuspend"


-- | @ontimeupdate@ attribute
ontimeupdate :: Attr
ontimeupdate = ident "ontimeupdate"


-- | @onundo@ attribute
onundo :: Attr
onundo = ident "onundo"


-- | @onunload@ attribute
onunload :: Attr
onunload = ident "onunload"


-- | @onvolumechange@ attribute
onvolumechange :: Attr
onvolumechange = ident "onvolumechange"


-- | @onwaiting@ attribute
onwaiting :: Attr
onwaiting = ident "onwaiting"


-- | @open@ attribute
open :: Attr
open = ident "open"


-- | @optimum@ attribute
optimum :: Attr
optimum = ident "optimum"


-- | @pattern@ attribute
pattern :: Attr
pattern = ident "pattern"


-- | @ping@ attribute
ping :: Attr
ping = ident "ping"


-- | @placeholder@ attribute
placeholder :: Attr
placeholder = ident "placeholder"


-- | @preload@ attribute
preload :: Attr
preload = ident "preload"


-- | @profile@ attribute
profile :: Attr
profile = ident "profile"


-- | @pubdate@ attribute
pubdate :: Attr
pubdate = ident "pubdate"


-- | @radiogroup@ attribute
radiogroup :: Attr
radiogroup = ident "radiogroup"


-- | @readonly@ attribute
readonly :: Attr
readonly = ident "readonly"


-- | @rel@ attribute
rel :: Attr
rel = ident "rel"


-- | @required@ attribute
required :: Attr
required = ident "required"


-- | @rev@ attribute
rev :: Attr
rev = ident "rev"


-- | @reversed@ attribute
reversed :: Attr
reversed = ident "reversed"


-- | @rows@ attribute
rows :: Attr
rows = ident "rows"


-- | @rowspan@ attribute
rowspan :: Attr
rowspan = ident "rowspan"


-- | @rules@ attribute
rules :: Attr
rules = ident "rules"


-- | @sandbox@ attribute
sandbox :: Attr
sandbox = ident "sandbox"


-- | @scheme@ attribute
scheme :: Attr
scheme = ident "scheme"


-- | @scope@ attribute
scope :: Attr
scope = ident "scope"


-- | @scoped@ attribute
scoped :: Attr
scoped = ident "scoped"


-- | @scrolling@ attribute
scrolling :: Attr
scrolling = ident "scrolling"


-- | @seamless@ attribute
seamless :: Attr
seamless = ident "seamless"


-- | @selected@ attribute
selected :: Attr
selected = ident "selected"


-- | @shape@ attribute
shape :: Attr
shape = ident "shape"


-- | @size@ attribute
size :: Attr
size = ident "size"


-- | @sizes@ attribute
sizes :: Attr
sizes = ident "sizes"


-- | @span@ attribute
span :: Attr
span = ident "span"


-- | @spellcheck@ attribute
spellcheck :: Attr
spellcheck = ident "spellcheck"


-- | @src@ attribute
src :: Attr
src = ident "src"


-- | @srcdoc@ attribute
srcdoc :: Attr
srcdoc = ident "srcdoc"


-- | @standby@ attribute
standby :: Attr
standby = ident "standby"


-- | @start@ attribute
start :: Attr
start = ident "start"


-- | @step@ attribute
step :: Attr
step = ident "step"


-- | @style@ attribute
style :: Attr
style = ident "style"


-- | @subject@ attribute
subject :: Attr
subject = ident "subject"


-- | @summary@ attribute
summary :: Attr
summary = ident "summary"


-- | @tabindex@ attribute
tabindex :: Attr
tabindex = ident "tabindex"


-- | @target@ attribute
target :: Attr
target = ident "target"


-- | @title@ attribute
title :: Attr
title = ident "title"


-- | @type'@ attribute
type' :: Attr
type' = ident "type"


-- | @usemap@ attribute
usemap :: Attr
usemap = ident "usemap"


-- | @valign@ attribute
valign :: Attr
valign = ident "valign"


-- | @value@ attribute
value :: Attr
value = ident "value"


-- | @valuetype@ attribute
valuetype :: Attr
valuetype = ident "valuetype"


-- | @vspace@ attribute
vspace :: Attr
vspace = ident "vspace"


-- | @width@ attribute
width :: Attr
width = ident "width"


-- | @wrap@ attribute
wrap :: Attr
wrap = ident "wrap"


-- | @xmlns@ attribute
xmlns :: Attr
xmlns = ident "xmlns"