#! /usr/bin/env python
# coding=utf-8
# vim:set foldmethod=marker:
import lispy
import logging
import platform
from PyQt4.QtCore import Qt

normal_map = {}
lispy.global_env.update({'normal-map':normal_map})
command_map = {}
lispy.global_env.update({'command-map':command_map})
search_map = {}
lispy.global_env.update({'search-map':search_map})

# 文字列->Qtの値変換Map{{{
_qt_keymap = {
    'Escape': Qt.Key_Escape,
    'Tab': Qt.Key_Tab,
    'Backtab': Qt.Key_Backtab,
    'Backspace': Qt.Key_Backspace,
    'Return': Qt.Key_Return,
    'Enter': Qt.Key_Enter,
    'Insert': Qt.Key_Insert,
    'Delete': Qt.Key_Delete,
    'Pause': Qt.Key_Pause,
    'Print': Qt.Key_Print,
    'SysReq': Qt.Key_SysReq,
    'Clear': Qt.Key_Clear,
    'Home': Qt.Key_Home,
    'End': Qt.Key_End,
    'Left': Qt.Key_Left,
    'Up': Qt.Key_Up,
    'Right': Qt.Key_Right,
    'Down': Qt.Key_Down,
    'PageUp': Qt.Key_PageUp,
    'PageDown': Qt.Key_PageDown,
    'Shift': Qt.Key_Shift,
    'Control': Qt.Key_Control,
    'Meta': Qt.Key_Meta,
    'Alt': Qt.Key_Alt,
    'AltGr': Qt.Key_AltGr,
    'CapsLock': Qt.Key_CapsLock,
    'NumLock': Qt.Key_NumLock,
    'ScrollLock': Qt.Key_ScrollLock,
    'F1': Qt.Key_F1,
    'F2': Qt.Key_F2,
    'F3': Qt.Key_F3,
    'F4': Qt.Key_F4,
    'F5': Qt.Key_F5,
    'F6': Qt.Key_F6,
    'F7': Qt.Key_F7,
    'F8': Qt.Key_F8,
    'F9': Qt.Key_F9,
    'F10': Qt.Key_F10,
    'F11': Qt.Key_F11,
    'F12': Qt.Key_F12,
    'F13': Qt.Key_F13,
    'F14': Qt.Key_F14,
    'F15': Qt.Key_F15,
    'F16': Qt.Key_F16,
    'F17': Qt.Key_F17,
    'F18': Qt.Key_F18,
    'F19': Qt.Key_F19,
    'F20': Qt.Key_F20,
    'F21': Qt.Key_F21,
    'F22': Qt.Key_F22,
    'F23': Qt.Key_F23,
    'F24': Qt.Key_F24,
    'F25': Qt.Key_F25,
    'F26': Qt.Key_F26,
    'F27': Qt.Key_F27,
    'F28': Qt.Key_F28,
    'F29': Qt.Key_F29,
    'F30': Qt.Key_F30,
    'F31': Qt.Key_F31,
    'F32': Qt.Key_F32,
    'F33': Qt.Key_F33,
    'F34': Qt.Key_F34,
    'F35': Qt.Key_F35,
    'Super_L': Qt.Key_Super_L,
    'Super_R': Qt.Key_Super_R,
    'Menu': Qt.Key_Menu,
    'Hyper_L': Qt.Key_Hyper_L,
    'Hyper_R': Qt.Key_Hyper_R,
    'Help': Qt.Key_Help,
    'Direction_L': Qt.Key_Direction_L,
    'Direction_R': Qt.Key_Direction_R,
    'Space': Qt.Key_Space,
    'Any': Qt.Key_Any,
    'Exclam': Qt.Key_Exclam,
    'QuoteDbl': Qt.Key_QuoteDbl,
    'NumberSign': Qt.Key_NumberSign,
    'Dollar': Qt.Key_Dollar,
    'Percent': Qt.Key_Percent,
    'Ampersand': Qt.Key_Ampersand,
    'Apostrophe': Qt.Key_Apostrophe,
    'ParenLeft': Qt.Key_ParenLeft,
    'ParenRight': Qt.Key_ParenRight,
    'Asterisk': Qt.Key_Asterisk,
    'Plus': Qt.Key_Plus,
    'Comma': Qt.Key_Comma,
    'Minus': Qt.Key_Minus,
    'Period': Qt.Key_Period,
    'Slash': Qt.Key_Slash,
    '0': Qt.Key_0,
    '1': Qt.Key_1,
    '2': Qt.Key_2,
    '3': Qt.Key_3,
    '4': Qt.Key_4,
    '5': Qt.Key_5,
    '6': Qt.Key_6,
    '7': Qt.Key_7,
    '8': Qt.Key_8,
    '9': Qt.Key_9,
    'Colon': Qt.Key_Colon,
    'Semicolon': Qt.Key_Semicolon,
    'Less': Qt.Key_Less,
    'Equal': Qt.Key_Equal,
    'Greater': Qt.Key_Greater,
    'Question': Qt.Key_Question,
    'At': Qt.Key_At,
    'a': Qt.Key_A,
    'b': Qt.Key_B,
    'c': Qt.Key_C,
    'd': Qt.Key_D,
    'e': Qt.Key_E,
    'f': Qt.Key_F,
    'g': Qt.Key_G,
    'h': Qt.Key_H,
    'i': Qt.Key_I,
    'j': Qt.Key_J,
    'k': Qt.Key_K,
    'l': Qt.Key_L,
    'm': Qt.Key_M,
    'n': Qt.Key_N,
    'o': Qt.Key_O,
    'p': Qt.Key_P,
    'q': Qt.Key_Q,
    'r': Qt.Key_R,
    's': Qt.Key_S,
    't': Qt.Key_T,
    'u': Qt.Key_U,
    'v': Qt.Key_V,
    'w': Qt.Key_W,
    'x': Qt.Key_X,
    'y': Qt.Key_Y,
    'z': Qt.Key_Z,
    'BracketLeft': Qt.Key_BracketLeft,
    'Backslash': Qt.Key_Backslash,
    'BracketRight': Qt.Key_BracketRight,
    'AsciiCircum': Qt.Key_AsciiCircum,
    'Underscore': Qt.Key_Underscore,
    'QuoteLeft': Qt.Key_QuoteLeft,
    'BraceLeft': Qt.Key_BraceLeft,
    'Bar': Qt.Key_Bar,
    'BraceRight': Qt.Key_BraceRight,
    'AsciiTilde': Qt.Key_AsciiTilde,
    'nobreakspace': Qt.Key_nobreakspace,
    'exclamdown': Qt.Key_exclamdown,
    'cent': Qt.Key_cent,
    'sterling': Qt.Key_sterling,
    'currency': Qt.Key_currency,
    'yen': Qt.Key_yen,
    'brokenbar': Qt.Key_brokenbar,
    'section': Qt.Key_section,
    'diaeresis': Qt.Key_diaeresis,
    'copyright': Qt.Key_copyright,
    'ordfeminine': Qt.Key_ordfeminine,
    'guillemotleft': Qt.Key_guillemotleft,
    'notsign': Qt.Key_notsign,
    'hyphen': Qt.Key_hyphen,
    'registered': Qt.Key_registered,
    'macron': Qt.Key_macron,
    'degree': Qt.Key_degree,
    'plusminus': Qt.Key_plusminus,
    'twosuperior': Qt.Key_twosuperior,
    'threesuperior': Qt.Key_threesuperior,
    'acute': Qt.Key_acute,
    'mu': Qt.Key_mu,
    'paragraph': Qt.Key_paragraph,
    'periodcentered': Qt.Key_periodcentered,
    'cedilla': Qt.Key_cedilla,
    'onesuperior': Qt.Key_onesuperior,
    'masculine': Qt.Key_masculine,
    'guillemotright': Qt.Key_guillemotright,
    'onequarter': Qt.Key_onequarter,
    'onehalf': Qt.Key_onehalf,
    'threequarters': Qt.Key_threequarters,
    'questiondown': Qt.Key_questiondown,
    'Agrave': Qt.Key_Agrave,
    'Aacute': Qt.Key_Aacute,
    'Acircumflex': Qt.Key_Acircumflex,
    'Atilde': Qt.Key_Atilde,
    'Adiaeresis': Qt.Key_Adiaeresis,
    'Aring': Qt.Key_Aring,
    'AE': Qt.Key_AE,
    'Ccedilla': Qt.Key_Ccedilla,
    'Egrave': Qt.Key_Egrave,
    'Eacute': Qt.Key_Eacute,
    'Ecircumflex': Qt.Key_Ecircumflex,
    'Ediaeresis': Qt.Key_Ediaeresis,
    'Igrave': Qt.Key_Igrave,
    'Iacute': Qt.Key_Iacute,
    'Icircumflex': Qt.Key_Icircumflex,
    'Idiaeresis': Qt.Key_Idiaeresis,
    'ETH': Qt.Key_ETH,
    'Ntilde': Qt.Key_Ntilde,
    'Ograve': Qt.Key_Ograve,
    'Oacute': Qt.Key_Oacute,
    'Ocircumflex': Qt.Key_Ocircumflex,
    'Otilde': Qt.Key_Otilde,
    'Odiaeresis': Qt.Key_Odiaeresis,
    'multiply': Qt.Key_multiply,
    'Ooblique': Qt.Key_Ooblique,
    'Ugrave': Qt.Key_Ugrave,
    'Uacute': Qt.Key_Uacute,
    'Ucircumflex': Qt.Key_Ucircumflex,
    'Udiaeresis': Qt.Key_Udiaeresis,
    'Yacute': Qt.Key_Yacute,
    'THORN': Qt.Key_THORN,
    'ssharp': Qt.Key_ssharp,
    'division': Qt.Key_division,
    'ydiaeresis': Qt.Key_ydiaeresis,
    'Multi_key': Qt.Key_Multi_key,
    'Codeinput': Qt.Key_Codeinput,
    'SingleCandidate': Qt.Key_SingleCandidate,
    'MultipleCandidate': Qt.Key_MultipleCandidate,
    'PreviousCandidate': Qt.Key_PreviousCandidate,
    'Mode_switch': Qt.Key_Mode_switch,
    'Kanji': Qt.Key_Kanji,
    'Muhenkan': Qt.Key_Muhenkan,
    'Henkan': Qt.Key_Henkan,
    'Romaji': Qt.Key_Romaji,
    'Hiragana': Qt.Key_Hiragana,
    'Katakana': Qt.Key_Katakana,
    'Hiragana_Katakana': Qt.Key_Hiragana_Katakana,
    'Zenkaku': Qt.Key_Zenkaku,
    'Hankaku': Qt.Key_Hankaku,
    'Zenkaku_Hankaku': Qt.Key_Zenkaku_Hankaku,
    'Touroku': Qt.Key_Touroku,
    'Massyo': Qt.Key_Massyo,
    'Kana_Lock': Qt.Key_Kana_Lock,
    'Kana_Shift': Qt.Key_Kana_Shift,
    'Eisu_Shift': Qt.Key_Eisu_Shift,
    'Eisu_toggle': Qt.Key_Eisu_toggle,
    'Hangul': Qt.Key_Hangul,
    'Hangul_Start': Qt.Key_Hangul_Start,
    'Hangul_End': Qt.Key_Hangul_End,
    'Hangul_Hanja': Qt.Key_Hangul_Hanja,
    'Hangul_Jamo': Qt.Key_Hangul_Jamo,
    'Hangul_Romaja': Qt.Key_Hangul_Romaja,
    'Hangul_Jeonja': Qt.Key_Hangul_Jeonja,
    'Hangul_Banja': Qt.Key_Hangul_Banja,
    'Hangul_PreHanja': Qt.Key_Hangul_PreHanja,
    'Hangul_PostHanja': Qt.Key_Hangul_PostHanja,
    'Hangul_Special': Qt.Key_Hangul_Special,
    'Dead_Grave': Qt.Key_Dead_Grave,
    'Dead_Acute': Qt.Key_Dead_Acute,
    'Dead_Circumflex': Qt.Key_Dead_Circumflex,
    'Dead_Tilde': Qt.Key_Dead_Tilde,
    'Dead_Macron': Qt.Key_Dead_Macron,
    'Dead_Breve': Qt.Key_Dead_Breve,
    'Dead_Abovedot': Qt.Key_Dead_Abovedot,
    'Dead_Diaeresis': Qt.Key_Dead_Diaeresis,
    'Dead_Abovering': Qt.Key_Dead_Abovering,
    'Dead_Doubleacute': Qt.Key_Dead_Doubleacute,
    'Dead_Caron': Qt.Key_Dead_Caron,
    'Dead_Cedilla': Qt.Key_Dead_Cedilla,
    'Dead_Ogonek': Qt.Key_Dead_Ogonek,
    'Dead_Iota': Qt.Key_Dead_Iota,
    'Dead_Voiced_Sound': Qt.Key_Dead_Voiced_Sound,
    'Dead_Semivoiced_Sound': Qt.Key_Dead_Semivoiced_Sound,
    'Dead_Belowdot': Qt.Key_Dead_Belowdot,
    'Dead_Hook': Qt.Key_Dead_Hook,
    'Dead_Horn': Qt.Key_Dead_Horn,
    'Back': Qt.Key_Back,
    'Forward': Qt.Key_Forward,
    'Stop': Qt.Key_Stop,
    'Refresh': Qt.Key_Refresh,
    'VolumeDown': Qt.Key_VolumeDown,
    'VolumeMute': Qt.Key_VolumeMute,
    'VolumeUp': Qt.Key_VolumeUp,
    'BassBoost': Qt.Key_BassBoost,
    'BassUp': Qt.Key_BassUp,
    'BassDown': Qt.Key_BassDown,
    'TrebleUp': Qt.Key_TrebleUp,
    'TrebleDown': Qt.Key_TrebleDown,
    'MediaPlay': Qt.Key_MediaPlay,
    'MediaStop': Qt.Key_MediaStop,
    'MediaPrevious': Qt.Key_MediaPrevious,
    'MediaNext': Qt.Key_MediaNext,
    'MediaRecord': Qt.Key_MediaRecord,
    'MediaPause': Qt.Key_MediaPause,
    'MediaTogglePlayPause': Qt.Key_MediaTogglePlayPause,
    'HomePage': Qt.Key_HomePage,
    'Favorites': Qt.Key_Favorites,
    'Search': Qt.Key_Search,
    'Standby': Qt.Key_Standby,
    'OpenUrl': Qt.Key_OpenUrl,
    'LaunchMail': Qt.Key_LaunchMail,
    'LaunchMedia': Qt.Key_LaunchMedia,
    'Launch0': Qt.Key_Launch0,
    'Launch1': Qt.Key_Launch1,
    'Launch2': Qt.Key_Launch2,
    'Launch3': Qt.Key_Launch3,
    'Launch4': Qt.Key_Launch4,
    'Launch5': Qt.Key_Launch5,
    'Launch6': Qt.Key_Launch6,
    'Launch7': Qt.Key_Launch7,
    'Launch8': Qt.Key_Launch8,
    'Launch9': Qt.Key_Launch9,
    'LaunchA': Qt.Key_LaunchA,
    'LaunchB': Qt.Key_LaunchB,
    'LaunchC': Qt.Key_LaunchC,
    'LaunchD': Qt.Key_LaunchD,
    'LaunchE': Qt.Key_LaunchE,
    'LaunchF': Qt.Key_LaunchF,
    'LaunchG': Qt.Key_LaunchG,
    'LaunchH': Qt.Key_LaunchH,
    'MonBrightnessUp': Qt.Key_MonBrightnessUp,
    'MonBrightnessDown': Qt.Key_MonBrightnessDown,
    'KeyboardLightOnOff': Qt.Key_KeyboardLightOnOff,
    'KeyboardBrightnessUp': Qt.Key_KeyboardBrightnessUp,
    'KeyboardBrightnessDown': Qt.Key_KeyboardBrightnessDown,
    'PowerOff': Qt.Key_PowerOff,
    'WakeUp': Qt.Key_WakeUp,
    'Eject': Qt.Key_Eject,
    'ScreenSaver': Qt.Key_ScreenSaver,
    'WWW': Qt.Key_WWW,
    'Memo': Qt.Key_Memo,
    'LightBulb': Qt.Key_LightBulb,
    'Shop': Qt.Key_Shop,
    'History': Qt.Key_History,
    'AddFavorite': Qt.Key_AddFavorite,
    'HotLinks': Qt.Key_HotLinks,
    'BrightnessAdjust': Qt.Key_BrightnessAdjust,
    'Finance': Qt.Key_Finance,
    'Community': Qt.Key_Community,
    'AudioRewind': Qt.Key_AudioRewind,
    'BackForward': Qt.Key_BackForward,
    'ApplicationLeft': Qt.Key_ApplicationLeft,
    'ApplicationRight': Qt.Key_ApplicationRight,
    'Book': Qt.Key_Book,
    'CD': Qt.Key_CD,
    'Calculator': Qt.Key_Calculator,
    'ToDoList': Qt.Key_ToDoList,
    'ClearGrab': Qt.Key_ClearGrab,
    'Close': Qt.Key_Close,
    'Copy': Qt.Key_Copy,
    'Cut': Qt.Key_Cut,
    'Display': Qt.Key_Display,
    'DOS': Qt.Key_DOS,
    'Documents': Qt.Key_Documents,
    'Excel': Qt.Key_Excel,
    'Explorer': Qt.Key_Explorer,
    'Game': Qt.Key_Game,
    'Go': Qt.Key_Go,
    'iTouch': Qt.Key_iTouch,
    'LogOff': Qt.Key_LogOff,
    'Market': Qt.Key_Market,
    'Meeting': Qt.Key_Meeting,
    'MenuKB': Qt.Key_MenuKB,
    'MenuPB': Qt.Key_MenuPB,
    'MySites': Qt.Key_MySites,
    'News': Qt.Key_News,
    'OfficeHome': Qt.Key_OfficeHome,
    'Option': Qt.Key_Option,
    'Paste': Qt.Key_Paste,
    'Phone': Qt.Key_Phone,
    'Calendar': Qt.Key_Calendar,
    'Reply': Qt.Key_Reply,
    'Reload': Qt.Key_Reload,
    'RotateWindows': Qt.Key_RotateWindows,
    'RotationPB': Qt.Key_RotationPB,
    'RotationKB': Qt.Key_RotationKB,
    'Save': Qt.Key_Save,
    'Send': Qt.Key_Send,
    'Spell': Qt.Key_Spell,
    'SplitScreen': Qt.Key_SplitScreen,
    'Support': Qt.Key_Support,
    'TaskPane': Qt.Key_TaskPane,
    'Terminal': Qt.Key_Terminal,
    'Tools': Qt.Key_Tools,
    'Travel': Qt.Key_Travel,
    'Video': Qt.Key_Video,
    'Word': Qt.Key_Word,
    'Xfer': Qt.Key_Xfer,
    'ZoomIn': Qt.Key_ZoomIn,
    'ZoomOut': Qt.Key_ZoomOut,
    'Away': Qt.Key_Away,
    'Messenger': Qt.Key_Messenger,
    'WebCam': Qt.Key_WebCam,
    'MailForward': Qt.Key_MailForward,
    'Pictures': Qt.Key_Pictures,
    'Music': Qt.Key_Music,
    'Battery': Qt.Key_Battery,
    'Bluetooth': Qt.Key_Bluetooth,
    'WLAN': Qt.Key_WLAN,
    'UWB': Qt.Key_UWB,
    'AudioForward': Qt.Key_AudioForward,
    'AudioRepeat': Qt.Key_AudioRepeat,
    'AudioRandomPlay': Qt.Key_AudioRandomPlay,
    'Subtitle': Qt.Key_Subtitle,
    'AudioCycleTrack': Qt.Key_AudioCycleTrack,
    'Time': Qt.Key_Time,
    'Hibernate': Qt.Key_Hibernate,
    'View': Qt.Key_View,
    'TopMenu': Qt.Key_TopMenu,
    'PowerDown': Qt.Key_PowerDown,
    'Suspend': Qt.Key_Suspend,
    'ContrastAdjust': Qt.Key_ContrastAdjust,
    'MediaLast': Qt.Key_MediaLast,
    'unknown': Qt.Key_unknown,
    'Call': Qt.Key_Call,
    'Camera': Qt.Key_Camera,
    'CameraFocus': Qt.Key_CameraFocus,
    'Context1': Qt.Key_Context1,
    'Context2': Qt.Key_Context2,
    'Context3': Qt.Key_Context3,
    'Context4': Qt.Key_Context4,
    'Flip': Qt.Key_Flip,
    'Hangup': Qt.Key_Hangup,
    'No': Qt.Key_No,
    'Select': Qt.Key_Select,
    'Yes': Qt.Key_Yes,
    'ToggleCallHangup': Qt.Key_ToggleCallHangup,
    'VoiceDial': Qt.Key_VoiceDial,
    'LastNumberRedial': Qt.Key_LastNumberRedial,
    'Execute': Qt.Key_Execute,
    'Printer': Qt.Key_Printer,
    'Play': Qt.Key_Play,
    'Sleep': Qt.Key_Sleep,
    'Zoom': Qt.Key_Zoom,
    'Cancel': Qt.Key_Cancel,
}

if "Darwin" == platform.system():
    _modifier_map = {
            'S': Qt.ShiftModifier,
            'M': Qt.ControlModifier,
            'A': Qt.AltModifier,
            'C': Qt.MetaModifier,
            }
else:
    _modifier_map = {
            'S': Qt.ShiftModifier,
            'C': Qt.ControlModifier,
            'A': Qt.AltModifier,
            'M': Qt.MetaModifier,
            }
#}}}

class Key(object):
    def __init__(self, key, modifiers):
        self.key = key
        self.modifiers = modifiers

    def __eq__(self, other):
        return self.key == other.key and self.modifiers == other.modifiers

    def __hash__(self):
        hsh = 1
        hsh = hsh * 31 + (self.key.__hash__() if self.key is not None else 0)
        hsh = hsh * 31 + int(self.modifiers)
        return hsh

def add_keymap(keymap, key, symbol):
    keymap[parse(key)] = symbol

def get_symbol(keymap, key):
    if key in keymap:
        return keymap[key]
    else:
        return None

def parse(keystr):
    keys = keystr.split('-')
    key = _qt_keymap[keys.pop(-1)]
    modifiers = Qt.NoModifier
    for k in keys:
        modifiers |= _modifier_map[k]
    return Key(key, modifiers)

def do_keymap(kmap, key):
    symbol = kmap.get(key)
    if symbol is not None:
        logging.debug('(' + lispy.to_string(symbol) + ')')
        lispy.eval([symbol])
        return True
    else:
        return False

