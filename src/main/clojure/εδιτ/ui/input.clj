(ns εδιτ.ui.input
  (:use [midje.sweet]
        [lamina.core])
  (:import [java.awt.event KeyListener KeyEvent]))

(def ^:private keycode-map
  {#{}
   {KeyEvent/VK_A \a KeyEvent/VK_B \b
    KeyEvent/VK_C \c KeyEvent/VK_D \d
    KeyEvent/VK_E \e KeyEvent/VK_F \f
    KeyEvent/VK_G \g KeyEvent/VK_H \h KeyEvent/VK_I \i
    KeyEvent/VK_J \j KeyEvent/VK_K \k KeyEvent/VK_L \l
    KeyEvent/VK_M \m KeyEvent/VK_N \n KeyEvent/VK_O \o
    KeyEvent/VK_P \p KeyEvent/VK_Q \q KeyEvent/VK_R \r
    KeyEvent/VK_S \s KeyEvent/VK_T \t KeyEvent/VK_U \u
    KeyEvent/VK_W \w KeyEvent/VK_V \v KeyEvent/VK_X \x
    KeyEvent/VK_Y \y KeyEvent/VK_Z \z
    KeyEvent/VK_0 \0 KeyEvent/VK_1 \1 KeyEvent/VK_2 \2
    KeyEvent/VK_3 \3 KeyEvent/VK_4 \4 KeyEvent/VK_5 \5
    KeyEvent/VK_6 \6 KeyEvent/VK_7 \7 KeyEvent/VK_8 \8
    KeyEvent/VK_9 \9
    KeyEvent/VK_SHIFT :shift  KeyEvent/VK_CONTROL :control
    KeyEvent/VK_ALT   :option KeyEvent/VK_META    :command
    KeyEvent/VK_TAB :tab KeyEvent/VK_ENTER :return
    KeyEvent/VK_BACK_SPACE :delete
    KeyEvent/VK_OPEN_BRACKET \[ KeyEvent/VK_CLOSE_BRACKET \]
    KeyEvent/VK_QUOTE \'
    KeyEvent/VK_SPACE \ }
   #{:shift}
   {KeyEvent/VK_A \A KeyEvent/VK_B \B
    KeyEvent/VK_C \C KeyEvent/VK_D \D
    KeyEvent/VK_E \E KeyEvent/VK_F \F
    KeyEvent/VK_G \G KeyEvent/VK_H \H KeyEvent/VK_I \I
    KeyEvent/VK_J \J KeyEvent/VK_K \K KeyEvent/VK_L \L
    KeyEvent/VK_M \M KeyEvent/VK_N \N KeyEvent/VK_O \O
    KeyEvent/VK_P \P KeyEvent/VK_Q \Q KeyEvent/VK_R \R
    KeyEvent/VK_S \S KeyEvent/VK_T \T KeyEvent/VK_U \U
    KeyEvent/VK_W \W KeyEvent/VK_V \V KeyEvent/VK_X \X
    KeyEvent/VK_Y \Y KeyEvent/VK_Z \Z
    KeyEvent/VK_0 \u0029 KeyEvent/VK_1 \! KeyEvent/VK_2\@
    KeyEvent/VK_3 \# KeyEvent/VK_4 \$ KeyEvent/VK_5 \%
    KeyEvent/VK_6 \^ KeyEvent/VK_7 \& KeyEvent/VK_8 \*
    KeyEvent/VK_9 \u0028
    KeyEvent/VK_OPEN_BRACKET \{ KeyEvent/VK_CLOSE_BRACKET \}
    KeyEvent/VK_COMMA \< KeyEvent/VK_PERIOD \>
    KeyEvent/VK_QUOTE \"
    KeyEvent/VK_SHIFT :shift}
   #{:command}
   {KeyEvent/VK_A :command-A KeyEvent/VK_B :command-B
    KeyEvent/VK_C :command-C KeyEvent/VK_D :command-D
    KeyEvent/VK_E :command-E KeyEvent/VK_F :command-F
    KeyEvent/VK_G :command-G KeyEvent/VK_H :command-H KeyEvent/VK_I :command-I
    KeyEvent/VK_J :command-J KeyEvent/VK_K :command-K KeyEvent/VK_L :command-L
    KeyEvent/VK_M :command-M KeyEvent/VK_N :command-N KeyEvent/VK_O :command-O
    KeyEvent/VK_P :command-P KeyEvent/VK_Q :command-Q KeyEvent/VK_R :command-R
    KeyEvent/VK_S :command-S KeyEvent/VK_T :command-T KeyEvent/VK_U :command-U
    KeyEvent/VK_W :command-W KeyEvent/VK_V :command-V KeyEvent/VK_X :command-X
    KeyEvent/VK_Y :command-Y KeyEvent/VK_Z :command-Z
    KeyEvent/VK_META :command}})

(defn- has-awt-modifier [bitfield m]
  (not= 0 (bit-and bitfield m)))

(defn ->modifiers [e]
  (let [bitfield (.getModifiersEx e)
        shift (if (has-awt-modifier bitfield KeyEvent/SHIFT_DOWN_MASK) #{:shift} #{})
        command (if (has-awt-modifier bitfield KeyEvent/META_DOWN_MASK) #{:command} #{})
        ]
    (clojure.set/union shift command)))

(defn key-symbol [e]
  (let [keycode (.getKeyCode e)]
    (or (get-in keycode-map [(->modifiers e) keycode])
        (prn e))))

(defn awt-listener
  ([channel f]
     (proxy [KeyListener] []
       (keyPressed [e]
         (when-let [key (key-symbol e)]
           (when-let [mapped (f key)]
             (enqueue channel mapped))))
       (keyReleased [e])
       (keyTyped [e])))
  ([channel] (awt-listener channel identity)))

(defn- awt-modifier-mask [mod]
  (mod {:shift   KeyEvent/SHIFT_DOWN_MASK
        :control KeyEvent/CTRL_DOWN_MASK
        :option  KeyEvent/ALT_DOWN_MASK
        :command KeyEvent/META_DOWN_MASK}))

(def ^:private source (javax.swing.JPanel.))
(defn- event [keycode & modifiers]
  (KeyEvent. source KeyEvent/KEY_PRESSED 0
             (reduce bit-or 0 (map awt-modifier-mask modifiers)) keycode
             KeyEvent/CHAR_UNDEFINED KeyEvent/KEY_LOCATION_UNKNOWN))

(facts "about key-symbol"
       (fact "maps letters"
             (key-symbol (event KeyEvent/VK_A)) => \a
             (map #(key-symbol (event %))
                  [KeyEvent/VK_A KeyEvent/VK_B KeyEvent/VK_C
                   KeyEvent/VK_D KeyEvent/VK_E KeyEvent/VK_F
                   KeyEvent/VK_G KeyEvent/VK_H KeyEvent/VK_I
                   KeyEvent/VK_J KeyEvent/VK_K KeyEvent/VK_L
                   KeyEvent/VK_M KeyEvent/VK_N KeyEvent/VK_O
                   KeyEvent/VK_P KeyEvent/VK_Q KeyEvent/VK_R
                   KeyEvent/VK_S KeyEvent/VK_T KeyEvent/VK_U
                   KeyEvent/VK_W KeyEvent/VK_V KeyEvent/VK_X
                   KeyEvent/VK_Y KeyEvent/VK_Z])
             => (seq "abcdefghijklmnopqrstuwvxyz"))
       (fact "maps numbers"
             (key-symbol (event KeyEvent/VK_0)) => \0
             (map #(key-symbol (event %))
                  [KeyEvent/VK_0 KeyEvent/VK_1 KeyEvent/VK_2
                   KeyEvent/VK_3 KeyEvent/VK_4 KeyEvent/VK_5
                   KeyEvent/VK_6 KeyEvent/VK_7 KeyEvent/VK_8
                   KeyEvent/VK_9])
             => (seq "0123456789"))
       (fact "maps modifier keys"
             (map #(key-symbol (event %))
                  [KeyEvent/VK_SHIFT KeyEvent/VK_CONTROL
                   KeyEvent/VK_ALT   KeyEvent/VK_META])
             => (list :shift :control :option :command))
       (fact "maps action keys"
             (map #(key-symbol (event %))
                  [KeyEvent/VK_TAB KeyEvent/VK_ENTER
                   KeyEvent/VK_BACK_SPACE])
             => (list :tab :return :delete))
       (fact "maps delimiters"
             (map #(key-symbol (event %))
                  [KeyEvent/VK_OPEN_BRACKET
                   KeyEvent/VK_CLOSE_BRACKET
                   KeyEvent/VK_QUOTE])
             => (seq "[]'")
             (map #(key-symbol (event % :shift))
                  [KeyEvent/VK_OPEN_BRACKET
                   KeyEvent/VK_CLOSE_BRACKET
                   KeyEvent/VK_9 KeyEvent/VK_0
                   KeyEvent/VK_COMMA KeyEvent/VK_PERIOD
                   KeyEvent/VK_QUOTE])
             => (seq "{}()<>\""))
       )
