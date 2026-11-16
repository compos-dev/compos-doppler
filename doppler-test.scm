;;; doppler-test.scm --- Doppler index value previews.

(deftest 'doppler-index-hides-short-and-missing-values
  "a preview keeps up to ten characters at each end and never reveals a complete short secret"
  (lambda ()
    (check-equal! (doppler--elide-value "0123456789abcdefghijklmnopqrstuvwxyz")
                  "0123456789…qrstuvwxyz" "a long value shows ten characters each side")
    (check-equal! (doppler--elide-value "abcd123456789wxyz")
                  "abcd12…89wxyz" "a shorter value shows fewer, so five stay hidden")
    (check-equal! (doppler--elide-value "short-value")
                  "••••" "a short value stays hidden")
    (check-equal! (doppler--elide-value "0123456789abc")
                  "0123…9abc" "four each side is the smallest preview")
    (check-equal! (doppler--elide-value #f)
                  "••••" "a missing value stays hidden")))

;; A value with a line break drew its row on two lines, and every row below
;; it was off by one: the arrow keys moved to the wrong row.
(deftest 'an-elided-value-is-one-line
  "a secret value with line breaks elides to a single line"
  (lambda ()
    (let ((shown (doppler--elide-value "abcdefghij\nklmnop\r\nqrstuvwxyz0123")))
      (check-false! (string-contains? shown "\n") "no line feed")
      (check-false! (string-contains? shown "\r") "no carriage return"))))

;;; --- the app, against a fake doppler CLI -----------------------------------------
;;; These were doppler_test.exs. A shell script stands in for the CLI and
;;; writes each call it gets to a calls file, so a test can read what ran.

(define t--dp-dir (string-append (compos-home) "/zz-doppler"))
(define t--dp-calls (string-append t--dp-dir "/calls"))

(define t--dp-projects "[{\"name\":\"personal\",\"description\":\"Private\"},{\"name\":\"work\",\"description\":\"Work\"}]")
(define t--dp-configs "[{\"name\":\"dev\",\"environment\":\"Development\"},{\"name\":\"prod\",\"environment\":\"Production\"}]")
(define t--dp-download "{\"ANTHROPIC_API_KEY\":\"super-secret\",\"DATABASE_URL\":\"super-secret\",\"SENTRY_AUTH_TOKEN\":\"super-secret\"}")
(define t--dp-names "{\"ANTHROPIC_API_KEY\":{},\"DATABASE_URL\":{}}")

(define (t--dp-script)
  (string-append
    "#!/bin/sh\n"
    "printf '%s\\n' \"$*\" >> " t--dp-calls "\n"
    "case \"$1\" in\n"
    "  projects) printf '%s' '" t--dp-projects "' ;;\n"
    "  configs) printf '%s' '" t--dp-configs "' ;;\n"
    "  secrets)\n"
    "    case \"$2\" in\n"
    "      get) printf 'super-secret' ;;\n"
    "      download) printf '%s' '" t--dp-download "' ;;\n"
    "      set|delete) : ;;\n"
    "      *) printf '%s' '" t--dp-names "' ;;\n"
    "    esac ;;\n"
    "esac\n"))

(define (t--dp-calls-text)
  (if (file-exists? t--dp-calls) (read-file t--dp-calls) ""))

(define (t--dp-call-lines)
  (filter (lambda (l) (not (equal? l ""))) (string-split (t--dp-calls-text) "\n")))

;; THUNK runs in a fresh frame with the fake CLI as doppler-program.
(define (t--dp-with-cli thunk)
  (shell-command->string (string-append "rm -rf " t--dp-dir))
  (make-directory! t--dp-dir)
  (let ((script (string-append t--dp-dir "/doppler")))
    (write-file! script (t--dp-script))
    (shell-command->string (string-append "chmod 700 " script))
    (when (buffer-known? "*doppler*") (buffer-kill! "*doppler*"))
    (with-custom 'doppler-program script
      (lambda ()
        (with-custom 'key-doppler-project "personal"
          (lambda ()
            (with-custom 'key-doppler-config "dev"
              (lambda ()
                (doppler-forget!)
                (with-frame (lambda (frame) (thunk))))))))))
  (when (buffer-known? "*doppler*") (buffer-kill! "*doppler*"))
  (doppler-forget!)
  (shell-command->string (string-append "rm -rf " t--dp-dir)))

;; the clipboard is the client's; catch what a command puts on it
(define (t--dp-clipboard thunk)
  (let ((saved clipboard-put!) (got #f))
    (set! clipboard-put! (lambda (text) (set! got text)))
    (thunk)
    (set! clipboard-put! saved)
    got))

(define (t--dp-open!)
  (run-command "doppler")
  (switch-to-buffer! "*doppler*"))

(define (t--dp-answer! . answers)
  (for-each (lambda (a) (minibuffer-input! a) (minibuffer-confirm!)) answers))

(deftest 'doppler-mode-lists-names-without-fetching-values
  "the list asks for names only, and no value is drawn"
  (lambda ()
    (t--dp-with-cli
      (lambda ()
        (t--dp-open!)
        (let ((text (buffer-text "*doppler*")))
          (check-contains! text "Doppler  personal/dev" "the title")
          (check-contains! text "ANTHROPIC_API_KEY" "a name")
          (check-contains! text "DATABASE_URL" "another")
          (check-false! (string-contains? text "super-secret") "no value in the text")
          (check-contains! (t--dp-calls-text) "secrets --project personal --config dev --only-names --json"
                           "the names call")
          (check-false! (string-contains? (t--dp-calls-text) "secrets get") "no per-name get")
          (check-equal! (buffer-local-value 'mode-name "*doppler*") "doppler-mode" "the mode")
          (check-false! (string-contains? (value->string (buffer-local-value 'render-blocks "*doppler*"))
                                          "super-secret")
                        "no value in the blocks"))))))

(deftest 'doppler-opens-as-a-grouped-primary-buffer-never-a-popup
  "older desktops opened it as a popup; the command repairs that"
  (lambda ()
    (t--dp-with-cli
      (lambda ()
        (buffer-create "*doppler*")
        (buffer-set-local! "*doppler*" 'window-class "popup popup-right")
        (buffer-set-local! "*doppler*" 'window-style "--popup-size:38%")
        (run-command "doppler")
        (check-equal! (current-buffer) "*doppler*" "it is the current buffer")
        (check-equal! (group-name (buffer-group "*doppler*")) "doppler" "in its own group")
        (check-false! (buffer-local-value 'window-class "*doppler*") "no popup class")
        (check-false! (buffer-local-value 'window-style "*doppler*") "no popup style")))))

(deftest 'secret-copy-copies-only-the-current-value
  "the first miss warms the whole config in one download"
  (lambda ()
    (t--dp-with-cli
      (lambda ()
        (t--dp-open!)
        (check-equal! (t--dp-clipboard (lambda () (run-command "doppler-secret-copy")))
                      "super-secret" "the value is on the clipboard")
        (check-contains! (t--dp-calls-text) "secrets download" "from the download")
        (check-false! (string-contains? (buffer-text "*doppler*") "super-secret")
                      "and not in the buffer")))))

(deftest 'a-secret-value-is-one-doppler-process-per-config
  "a second name costs no process; a write drops that one entry"
  (lambda ()
    (t--dp-with-cli
      (lambda ()
        (check-equal! (doppler-secret-value "personal" "dev" "SENTRY_AUTH_TOKEN") "super-secret"
                      "the first value")
        (let ((fetched (length (t--dp-call-lines))))
          (check-equal! (doppler-secret-value "personal" "dev" "DATABASE_URL") "super-secret"
                        "a second name")
          (check-equal! (doppler-secret-value "personal" "dev" "SENTRY_AUTH_TOKEN") "super-secret"
                        "the first again")
          (check-equal! (length (t--dp-call-lines)) fetched "no new process"))
        (doppler-secret-set! "personal" "dev" "SENTRY_AUTH_TOKEN" "rotated")
        (doppler-secret-value "personal" "dev" "SENTRY_AUTH_TOKEN")
        (check-equal! (length (filter (lambda (l) (string-contains? l "secrets get SENTRY_AUTH_TOKEN"))
                                      (t--dp-call-lines)))
                      1 "the next read asks for that name alone")))))

(deftest 'row-clicks-select-and-action-clicks-run-the-commands
  "a click goes through the block-click hook, as the client sends it"
  (lambda ()
    (t--dp-with-cli
      (lambda ()
        (t--dp-open!)
        (run-hook-with-args-until-success 'block-click "*doppler*" "doppler:row:1")
        (check-equal! (list-current "*doppler*") "DATABASE_URL" "the row is selected")
        (check-equal! (t--dp-clipboard
                        (lambda () (run-hook-with-args-until-success 'block-click "*doppler*" "doppler:copy")))
                      "super-secret" "copy copies")
        (check-contains! (t--dp-calls-text) "secrets download" "from the download")
        (run-hook-with-args-until-success 'block-click "*doppler*" "doppler:add")
        (t--dp-answer! "mouse_key" "mouse value")
        (check-contains! (t--dp-calls-text) "secrets set MOUSE_KEY mouse value" "add sets")))))

(deftest 'ui-actions-is-catalogued-with-a-working-click-example
  "the action bar doppler draws is a catalogued component"
  (lambda ()
    (let ((description (value->string (describe-component 'ui/actions))))
      (check-contains! description "clickable actions" "the doc")
      (check-contains! description "refresh" "the example"))
    (check-contains! (value->string (component 'ui/actions '(actions (("go" "Go" "RET")))))
                     "click \"go\"" "an action carries its click")))

(deftest 'secret-add-sets-a-named-value-and-does-not-render-it
  "the name is upcased; the value goes to the CLI only"
  (lambda ()
    (t--dp-with-cli
      (lambda ()
        (t--dp-open!)
        (run-command "doppler-secret-add")
        (t--dp-answer! "new_token" "value with spaces")
        (check-contains! (t--dp-calls-text) "secrets set NEW_TOKEN value with spaces" "the set call")
        (check-contains! (t--dp-calls-text) "--silent --no-interactive" "quietly")
        (check-false! (string-contains? (buffer-text "*doppler*") "value with spaces")
                      "the value is not drawn")))))

(deftest 'flag-then-execute-confirms-before-deleting
  "nothing is deleted until the prompt says yes"
  (lambda ()
    (t--dp-with-cli
      (lambda ()
        (t--dp-open!)
        (run-command "list-flag-D")
        (run-command "list-execute")
        (check-false! (string-contains? (t--dp-calls-text) "secrets delete") "not yet")
        (t--dp-answer! "yes")
        (check-contains! (t--dp-calls-text) "secrets delete ANTHROPIC_API_KEY" "deleted")
        (check-contains! (t--dp-calls-text) "--yes --silent" "without a second CLI prompt")))))

(deftest 'select-project-changes-the-project-and-config-of-later-reads
  "the list reads the new project and config"
  (lambda ()
    (t--dp-with-cli
      (lambda ()
        (t--dp-open!)
        (run-command "doppler-select-project")
        (t--dp-answer! "work" "prod")
        (check-equal! (buffer-local-value 'doppler-project "*doppler*") "work" "the project")
        (check-equal! (buffer-local-value 'doppler-config "*doppler*") "prod" "the config")
        (check-contains! (buffer-text "*doppler*") "Doppler  work/prod" "the title")
        (check-contains! (t--dp-calls-text) "secrets --project work --config prod --only-names --json"
                         "the names call")))))

(deftest 'restoring-doppler-mode-rebuilds-its-names
  "a restored buffer keeps its project and config and draws fresh rows"
  (lambda ()
    (t--dp-with-cli
      (lambda ()
        (buffer-create "*doppler*")
        (buffer-append! "*doppler*" "stale secret value")
        (buffer-set-local! "*doppler*" 'doppler-project "work")
        (buffer-set-local! "*doppler*" 'doppler-config "prod")
        (with-current-buffer "*doppler*" (lambda () (set-mode! "doppler-mode")))
        (let ((text (buffer-text "*doppler*")))
          (check-contains! text "Doppler  work/prod" "the title")
          (check-contains! text "ANTHROPIC_API_KEY" "a name")
          (check-false! (string-contains? text "stale secret value") "the stale text is gone"))))))
