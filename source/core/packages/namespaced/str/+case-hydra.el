;;; core/modules/emacs/str/+case-hydra.el --- Hydra for String Cases -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-09-22
;; Timestamp:  2023-06-23
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Hydra for String Cases
;;
;;; Code:


(imp:require :str 'case)


;;------------------------------------------------------------------------------
;; Hydra
;;------------------------------------------------------------------------------

(setq int<str>:hydra:case:fmt
      (concat "%-" (format "%d" (length "Random (lOwEr or UpPeR)")) "s"))

;;------------------------------
;; Delay until after Hydra is loaded...
;;------------------------------
(imp:eval:after hydra
  (defhydra str:hydra:case (:color red  ;; Allow & quit on non-hydra-heads.
                            :hint none) ;; no hint - just docstr
    "
Convert Region To:
^^──────^^─────────────────────┬─^^──────────^^───────────────────┬─^^─────────^^───────────────────┬─^^───────────^^─────────────────┬───────────────────
^^simple^^                     │ ^^snake_case^^                   │ ^^CamelCase^^                   │ ^^AlTeRnAtInG^^                 │
^^──────^^─────────────────────┼─^^──────────^^───────────────────┼─^^─────────^^───────────────────┼─^^───────────^^─────────────────┼───────────────────
_l_: ?l? │ _sl_:  ?sl?^ │ _cl_: ?cl? │ _al_: ?al? │ _m_: ?m?
_u_: ?u? │ _su_:  ?su?^ │ _cu_: ?cu? │ _au_: ?au? │
_t_: ?t? │ _st_:  ?st?^ │ ^ ^  ^ ^                        │ _ar_: ?ar? │ _C-g_: ?C-g?
^ ^  ^ ^                       │ _s-l_: ?s-l? │ ^ ^  ^ ^                        │ ^ ^  ^ ^                        │
^ ^  ^ ^                       │ _s-u_: ?s-u? │ ^ ^  ^ ^                        │ ^ ^  ^ ^                        │
^ ^  ^ ^                       │ _s-t_: ?s-t? │ ^ ^  ^ ^                        │ ^ ^  ^ ^                        │
"
    ;;------------------------------
    ;; Simple Cases
    ;;------------------------------
    ("l" #'str:case/region:to:lower (format int<str>:hydra:case:fmt "lowercase") :exit t)
    ("u" #'str:case/region:to:upper (format int<str>:hydra:case:fmt "UPPERCASE") :exit t)
    ("t" #'str:case/region:to:title (format int<str>:hydra:case:fmt "Title Case") :exit t)

    ;;------------------------------
    ;; Snake_Cases
    ;;------------------------------
    ("sl" #'str:case/region:to:snake.lower (format int<str>:hydra:case:fmt "lower_snake_case") :exit t)
    ("su" #'str:case/region:to:snake.upper (format int<str>:hydra:case:fmt "UPPER_SNAKE_CASE") :exit t)
    ("st" #'str:case/region:to:snake.title (format int<str>:hydra:case:fmt "Title_Snake_Case") :exit t)

    ("s-l" #'str:case/region:to:snake.lower (format int<str>:hydra:case:fmt "lower-snake-case") :exit t)
    ("s-u" #'str:case/region:to:snake.upper (format int<str>:hydra:case:fmt "UPPER-SNAKE-CASE") :exit t)
    ("s-t" #'str:case/region:to:snake.title (format int<str>:hydra:case:fmt "Title-Snake-Case") :exit t)

    ;;------------------------------
    ;; CamelCases
    ;;------------------------------
    ("cl" #'str:case/region:to:camel.lower (format int<str>:hydra:case:fmt "lower_camel_case") :exit t)
    ("cu" #'str:case/region:to:camel.upper (format int<str>:hydra:case:fmt "UPPER_CAMEL_CASE") :exit t)
    ("ct" #'str:case/region:to:camel.title (format int<str>:hydra:case:fmt "Title_Camel_Case") :exit t)

    ;;------------------------------
    ;; Alternating_Cases
    ;;------------------------------
    ("al" #'str:case/region:to:alternating.lower (format int<str>:hydra:case:fmt "lOwEr AlTeRnAtInG cAsE") :exit t)
    ("au" #'str:case/region:to:alternating.upper (format int<str>:hydra:case:fmt "UpPeR aLtErNaTiNg CaSe") :exit t)
    ("ar" #'str:case/region:to:alternating.title (format int<str>:hydra:case:fmt "Random (lOwEr or UpPeR)") :exit t)
    ("at" #'str:cmd:case/region:toggle           (format int<str>:hydra:case:fmt "Toggle") :exit t)

    ;;------------------------------
    ;; Multiple Cases
    ;;------------------------------
    ("m" #'str:case/region:to (format int<str>:hydra:case:fmt "Multiple Cases") :exit t)

    ;;------------------------------
    ;; Explicit Quit
    ;;------------------------------
    ("C-g" nil (format int<str>:hydra:case:fmt "quit")))
  ;; (str:hydra:case/body)

  ;;------------------------------
  ;; /Delay
  ;;------------------------------
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :str '+hydra '+case)
