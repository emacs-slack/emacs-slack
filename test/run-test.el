;;; run-test.el --- ERT test suite runner -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-team)
(require 'slack-channel)
(require 'slack-usergroup)
(require 'slack-message-formatter)
(require 'slack-block)
(require 'slack-mrkdwn)
(require 'slack-message-sender)
(require 'slack-image)
(require 'slack-message-buffer)
(require 'slack-export)
(require 'slack-user-message)

(defvar slack-channel-button-keymap nil)
(defvar slack-user-mention-keymap nil)
(setq slack-render-image-p t)

(defmacro slack-test-setup (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((channel-id "C11111")
          (channel-name "TestChannel")
          (channel (make-instance 'slack-channel
                                  :id channel-id
                                  :name channel-name))
          (user-id "U11111")
          (user-name "TestUser")
          (real-name "RealName")
          (display-name "Display name")
          (user (list :name user-name :id user-id
                      :profile (list :display_name_normalized display-name
                                     :real_name_normalized real-name)))
          (usergroup-id "S88888")
          (usergroup-handle "TestUsergroup")
          (usergroup (make-instance 'slack-usergroup
                                    :id usergroup-id
                                    :handle usergroup-handle))
          (team (make-instance 'slack-team
                               :self-id "U38383838"
                               :channels (let ((h (make-hash-table :test 'equal)))
                                           (puthash (oref channel id)
                                                    channel
                                                    h)
                                           h)
                               :users (let ((h (make-hash-table :test 'equal)))
                                        (puthash (plist-get user :id)
                                                 user
                                                 h)
                                        h)
                               :usergroups (list usergroup))))
     ,@body))

(ert-deftest slack-test-image-path ()
  (let* ((url "http://example.com/image.jpg?crop=1:2;3:4")
         (splitted (split-string url "?"))
         (query (cadr splitted))
         (ext (file-name-extension (car splitted))))
    (should (equal (expand-file-name (concat (md5 url) "." ext)
                                     slack-image-file-directory)
                   (slack-image-path url))))
  (let* ((url "http://example.com/image.jpg")
         (ext "jpg"))
    (should (equal (expand-file-name (concat (md5 url) "." ext)
                                     slack-image-file-directory)
                   (slack-image-path url))))
  (let* ((url "https://qiita-user-contents.imgix.net/https%3A%2F%2Fcdn.qiita.com%2Fassets%2Fpublic%2Farticle-ogp-background-1150d8b18a7c15795b701a55ae908f94.png?ixlib=rb-1.2.2&w=1200&mark=https%3A%2F%2Fqiita-user-contents.imgix.net%2F~text%3Fixlib%3Drb-1.2.2%26w%3D840%26h%3D380%26txt%3DRuby%25E3%2581%25AESJIS%25E3%2581%25AFShift_JIS%25E3%2581%2598%25E3%2582%2583%25E3%2581%25AA%25E3%2581%2584%26txt-color%3D%2523333%26txt-font%3DAvenir-Black%26txt-size%3D54%26txt-clip%3Dellipsis%26txt-align%3Dcenter%252Cmiddle%26s%3D7bec43d35ef823368af1e72227127508&mark-align=center%2Cmiddle&blend=https%3A%2F%2Fqiita-user-contents.imgix.net%2F~text%3Fixlib%3Drb-1.2.2%26w%3D840%26h%3D500%26txt%3D%2540yugo-yamamoto%26txt-color%3D%2523333%26txt-font%3DAvenir-Black%26txt-size%3D45%26txt-align%3Dright%252Cbottom%26s%3D8d65eaf7282eb521ea4a47ecac74efc5&blend-align=center%2Cmiddle&blend-mode=normal&s=1b2bdc847358703b21238cb69b5f37d8")
         (splitted (split-string url "?"))
         (query (cadr splitted))
         (ext (file-name-extension (car splitted))))
    (should (equal (expand-file-name (concat (md5 url) "." ext)
                                     slack-image-file-directory)
                   (slack-image-path url)))))

(ert-deftest slack-test-unescape-&<> ()
  (should (equal "<" (slack-unescape-&<> "&lt;")))
  (should (equal ">" (slack-unescape-&<> "&gt;")))
  (should (equal "&" (slack-unescape-&<> "&amp;")))
  (should (equal "foo" (slack-unescape-&<> "foo"))))

(ert-deftest slack-test-unescape-channel ()
  (slack-test-setup
    (should (equal (format "#%s" channel-name)
                   (slack-unescape-channel
                    (format "<#%s>" channel-id)
                    team)))
    (should (equal "#Foo"
                   (slack-unescape-channel
                    (format "<#%s|Foo>" channel-id)
                    team)))
    (should (equal "#<Unknown CHANNEL>"
                   (slack-unescape-channel
                    "<#C9999999>" team)))))

(ert-deftest slack-test-unescape-@ ()
  (slack-test-setup
    (oset team full-and-display-names t)
    (should (equal (format "@%s" real-name)
                   (slack-unescape-@
                    (format "<@%s>" user-id)
                    team)))
    (should (equal (format "@%s" real-name)
                   (slack-unescape-@
                    (format "<@%s|Foo>" user-id)
                    team)))
    (should (equal "@<Unknown USER>"
                   (slack-unescape-@
                    "<@U424242>" team)))
    (oset team full-and-display-names nil)
    (should (equal (format "@%s" display-name)
                   (slack-unescape-@
                    (format "<@%s>" user-id)
                    team)))
    (should (equal (format "@%s" display-name)
                   (slack-unescape-@
                    (format "<@%s|Foo>" user-id)
                    team)))
    (should (equal "@<Unknown USER>"
                   (slack-unescape-@
                    "<@U424242>" team)))
    ))

(ert-deftest slack-test-unescape-@-mention-keymap ()
  "Mention text rendered by `slack-unescape-@' carries a `user-id'
text property and a keymap so it can be clicked to open a profile."
  (slack-test-setup
    (let* ((rendered (slack-unescape-@ (format "<@%s>" user-id) team))
           (pos (string-match (regexp-quote (format "@%s" display-name)) rendered)))
      (should (string= user-id (get-text-property pos 'user-id rendered)))
      (should (eq slack-user-mention-keymap
                 (get-text-property pos 'keymap rendered))))))

(ert-deftest slack-test-unescape-!subteam ()
  (slack-test-setup
    (should (equal (slack-unescape-!subteam
                    (format "<!subteam^%s|@%s>"
                            usergroup-id
                            usergroup-handle))
                   (format "@%s" usergroup-handle)))))


(ert-deftest slack-test-unescape-!date ()
  (should (equal (slack-unescape-!date
                  "<!date^1392734382^Posted {date_num} {time_secs}|Posted 2014-02-18 14:39:42>"
                  0)
                 "Posted 2014-02-18 14:39:42") )
  (should (equal (slack-unescape-!date
                  "<!date^1392734382^{date} at {time}|February 18 2014 at 14:39 PST>"
                  0)
                 "February 18, 2014 at 14:39"))
  (should (equal (slack-unescape-!date
                  "<!date^1392734382^{date_short}^https://example.com/|Feb 18, 2014 PST>"
                  0)
                 "<https://example.com/|Feb 18, 2014>"))
  )


(ert-deftest slack-test-unescape-variable ()
  (should (equal "@here" (slack-unescape-variable "<!here>")))
  (should (equal "@here" (slack-unescape-variable "<!here|here>")))
  (should (equal "@channel" (slack-unescape-variable "<!channel>")))
  (should (equal "@everyone" (slack-unescape-variable "<!everyone>")))
  (should (equal "<foo>" (slack-unescape-variable "<!foo>")))
  (should (equal "<label>" (slack-unescape-variable "<!foo|label>"))))

(ert-deftest slack-test-section-layout-block ()
  (let* ((input '(:type "section" :block_id "Vgv" :text (:type "mrkdwn" :text "Take a look at this image." :verbatim :json-false) :accessory (:fallback "600x800px image" :image_url "https://api.slack.com/img/blocks/bkb_template_images/palmtree.png" :image_width 600 :image_height 800 :image_bytes 482870 :type "image" :alt_text "palm tree") :fields ((:type "mrkdwn" :text "Foo" :verbatim :json-false) (:type "mrkdwn" :text "Bar" :verbatim :json-false))))
         (out (slack-create-layout-block input))
         (text (oref out text)))
    (should (eq 'slack-section-layout-block
                (eieio-object-class-name out)))
    (should (equal "Vgv" (oref out block-id)))
    (should (eq (eieio-object-class-name text)
                'slack-text-message-composition-object))
    (should (eq 2 (length (oref out fields))))
    (mapc #'(lambda (e) (should (eq 'slack-text-message-composition-object
                                    (eieio-object-class-name e))))
          (oref out fields))
    (should (eq 'slack-image-block-element
                (eieio-object-class-name (oref out accessory))))
    ))

(ert-deftest slack-test-divider-layout-block ()
  (let* ((input '(:type "divider" :block_id "y5a"))
         (out (slack-create-layout-block input)))
    (should (eq 'slack-divider-layout-block
                (eieio-object-class-name out)))
    (should (equal (oref out block-id)
                   "y5a"))))

(ert-deftest slack-test-image-layout-block ()
  (let* ((input '(:type "image" :block_id "Tron" :image_url "https://api.slack.com/img/blocks/bkb_template_images/beagle.png" :alt_text "image1" :title (:type "plain_text" :text "image1" :emoji t) :fallback "1080x1080px image" :image_width 1080 :image_height 1080 :image_bytes 1432686))
         (out (slack-create-layout-block input)))
    (should (eq 'slack-image-layout-block
                (eieio-object-class-name out)))
    (should (eq 1080 (oref out image-width)))
    (should (eq 1080 (oref out image-height)))
    (should (equal
             "https://api.slack.com/img/blocks/bkb_template_images/beagle.png"
             (oref out image-url)))

    (should (equal "image1" (oref (oref out title) text)))
    (should (eq t (oref (oref out title) emoji)))
    ))

(ert-deftest slack-test-actions-layout-block ()
  (let* ((input '(:type
                  "actions"
                  :block_id "lzhj"
                  :elements ((:type
                              "conversations_select"
                              :action_id "r4Y"
                              :placeholder (:type
                                            "plain_text"
                                            :text "Select a conversation"
                                            :emoji t)))))
         (out (slack-create-layout-block input)))
    (should (eq 'slack-actions-layout-block
                (eieio-object-class-name out)))
    (should (eq 1 (length (oref out elements))))
    (should (equal "lzhj" (oref out block-id)))
    ))

(ert-deftest slack-text-context-layout-block ()
  (let* ((input '(:type "context" :block_id "mOfwN" :elements ((:type "plain_text" :text "Last updated: Jan 1, 2019" :emoji t) (:fallback "600x800px image" :image_url "https://api.slack.com/img/blocks/bkb_template_images/goldengate.png" :image_width 600 :image_height 800 :image_bytes 828593 :type "image" :alt_text "goldengate"))))
         (out (slack-create-layout-block input)))
    (should (eq 'slack-context-layout-block
                (eieio-object-class-name out)))
    (should (equal "mOfwN" (oref out block-id)))
    (should (eq 2 (length (oref out elements))))
    ))

(ert-deftest slack-test-button-block-element ()
  (let* ((input '(:type "section" :block_id "mL2Z" :text (:type "mrkdwn" :text "You can add a button alongside text in your message. " :verbatim :json-false) :accessory (:type "button" :text (:type "plain_text" :text "Button" :emoji t) :value "click_me_123" :action_id "4sDY")))
         (out (slack-create-layout-block input))
         (button (oref out accessory)))
    (should (eq 'slack-button-block-element
                (eieio-object-class-name button)))
    (should (equal "click_me_123"
                   (oref button value)))
    (should (equal "4sDY"
                   (oref button action-id)))
    (should (equal "Button"
                   (oref (oref button text) text)))
    ))

(ert-deftest slack-test-static-select-block-element ()
  (let* ((input '(:type "section" :block_id "1ljQm" :text (:type "mrkdwn" :text "Pick an item from the dropdown list" :verbatim :json-false) :accessory (:type "static_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :options ((:text (:type "plain_text" :text "Choice 1" :emoji t) :value "value-0") (:text (:type "plain_text" :text "Choice 2" :emoji t) :value "value-1") (:text (:type "plain_text" :text "Choice 3" :emoji t) :value "value-2")) :action_id "glxm" :initial_option (:text (:type "plain_text" :text "Choice 2" :emoji t) :value "value-1"))))
         (out (slack-create-layout-block input))
         (select (oref out accessory)))
    (should (eq 'slack-static-select-block-element
                (eieio-object-class-name select)))
    (should (eq 3 (length (oref select options))))
    (should (eq nil (oref select option-groups)))
    (should (equal "glxm" (oref select action-id)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref select placeholder))))
    (should (eq 'slack-option-message-composition-object
                (eieio-object-class-name (oref select initial-option)))))

  (let* ((input '(:type "section" :block_id "2aQr" :text (:type "mrkdwn" :text "Pick an item from the dropdown list" :verbatim :json-false) :accessory (:type "static_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :initial_option (:text (:type "plain_text" :text "Choice 1" :emoji t) :value "value-0") :option_groups ((:label (:type "plain_text" :text "Foo" :emoji t) :options ((:text (:type "plain_text" :text "Choice 1" :emoji t) :value "value-0") (:text (:type "plain_text" :text "Choice 2" :emoji t) :value "value-1") (:text (:type "plain_text" :text "Choice 3" :emoji t) :value "value-2"))) (:label (:type "plain_text" :text "Bar" :emoji t) :options ((:text (:type "plain_text" :text "Choice 1" :emoji t) :value "value-0") (:text (:type "plain_text" :text "Choice 2" :emoji t) :value "value-1") (:text (:type "plain_text" :text "Choice 3" :emoji t) :value "value-2")))) :action_id "rUen5")))
         (out (slack-create-layout-block input))
         (select (oref out accessory)))
    (should (eq 'slack-static-select-block-element
                (eieio-object-class-name select)))
    (should (eq nil (oref select options)))
    (should (eq 2 (length (oref select option-groups))))
    (should (eq 'slack-option-message-composition-object
                (eieio-object-class-name (oref select initial-option))))
    (should (equal "rUen5" (oref select action-id)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref select placeholder))))))

(ert-deftest slack-test-external-select-block-element ()
  (let* ((input '(:type "section" :block_id "7y4+" :text (:type "mrkdwn" :text "Pick an item from the dropdown list" :verbatim :json-false) :accessory (:type "external_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :min_query_length 3 :action_id "03T+" :confirm (:title (:type "plain_text" :text "Title") :text (:type "plain_text" :text "Text") :confirm (:type "plain_text" :text "Yes") :deny (:type "plain_text" :text "No")))))
         (out (slack-create-layout-block input))
         (select (oref out accessory)))
    (should (eq 'slack-external-select-block-element
                (eieio-object-class-name select)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref select placeholder))))
    (should (eq 'slack-confirmation-dialog-message-composition-object
                (eieio-object-class-name (oref select confirm))))
    (should (eq 3 (oref select min-query-length)))
    (should (equal "03T+" (oref select action-id)))))

(ert-deftest slack-test-user-select-block-element ()
  (let* ((input '(:type "section" :block_id "DjXiH" :text (:type "mrkdwn" :text "Pick an item from the dropdown list" :verbatim :json-false) :accessory (:type "users_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :confirm (:title (:type "plain_text" :text "Title" :emoji t) :text (:type "plain_text" :text "Text" :emoji t) :confirm (:type "plain_text" :text "Yes" :emoji t) :deny (:type "plain_text" :text "No" :emoji t)) :action_id "VtT" :initial_user "UAAAAA")))
         (out (slack-create-layout-block input))
         (select (oref out accessory)))
    (should (eq 'slack-user-select-block-element
                (eieio-object-class-name select)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref select placeholder))))
    (should (eq 'slack-confirmation-dialog-message-composition-object
                (eieio-object-class-name (oref select confirm))))
    (should (equal "VtT" (oref select action-id)))
    (should (equal "UAAAAA" (oref select initial-user)))))

(ert-deftest slack-test-conversation-select-block-element ()
  (let* ((input '(:type "section" :block_id "SrLD" :text (:type "mrkdwn" :text "Pick an item from the dropdown list" :verbatim :json-false) :accessory (:type "conversations_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :confirm (:title (:type "plain_text" :text "Title" :emoji t) :text (:type "plain_text" :text "Text" :emoji t) :confirm (:type "plain_text" :text "Yes" :emoji t) :deny (:type "plain_text" :text "No" :emoji t)) :action_id "GfRre")))
         (out (slack-create-layout-block input))
         (select (oref out accessory)))
    (should (eq 'slack-conversation-select-block-element
                (eieio-object-class-name select)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref select placeholder))))
    (should (eq 'slack-confirmation-dialog-message-composition-object
                (eieio-object-class-name (oref select confirm))))
    (should (equal "GfRre" (oref select action-id)))))

(ert-deftest slack-test-channel-select-block-element ()
  (let* ((input '(:type "section" :block_id "fBa" :text (:type "mrkdwn" :text "Pick an item from the dropdown list" :verbatim :json-false) :accessory (:type "channels_select" :initial_channel "C0G31N06B" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :action_id "zJl")))
         (out (slack-create-layout-block input))
         (select (oref out accessory)))
    (should (eq 'slack-channel-select-block-element
                (eieio-object-class-name select)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref select placeholder))))
    (should (equal "C0G31N06B" (oref select initial-channel)))
    (should (equal "zJl" (oref select action-id)))))

(ert-deftest slack-test-overflow-block-element ()
  (let* ((input '(:type "section" :block_id "Egl" :text (:type "mrkdwn" :text "This block has an overflow menu." :verbatim :json-false) :accessory (:type "overflow" :options ((:text (:type "plain_text" :text "Option 1" :emoji t) :value "value-0") (:text (:type "plain_text" :text "Option 2" :emoji t) :value "value-1") (:text (:type "plain_text" :text "Option 3" :emoji t) :value "value-2") (:text (:type "plain_text" :text "Option 4" :emoji t) :value "value-3")) :confirm (:title (:type "plain_text" :text "Title" :emoji t) :text (:type "plain_text" :text "Text" :emoji t) :confirm (:type "plain_text" :text "Yes" :emoji t) :deny (:type "plain_text" :text "No" :emoji t)) :action_id "ksRP")))
         (out (slack-create-layout-block input))
         (overflow (oref out accessory)))
    (should (eq 'slack-overflow-menu-block-element
                (eieio-object-class-name overflow)))
    (should (eq 'slack-confirmation-dialog-message-composition-object
                (eieio-object-class-name (oref overflow confirm))))
    (should (eq 4 (length (oref overflow options))))
    (should (equal "ksRP" (oref overflow action-id)))))

(ert-deftest slack-test-datepicker-block-element ()
  (let* ((input '(:type "section" :block_id "VvZ" :text (:type "mrkdwn" :text "Pick a date for the deadline." :verbatim :json-false) :accessory (:type "datepicker" :initial_date "1990-04-28" :placeholder (:type "plain_text" :text "Select a date" :emoji t) :action_id "G=RRF")))
         (out (slack-create-layout-block input))
         (datepicker (oref out accessory)))
    (should (eq 'slack-date-picker-block-element
                (eieio-object-class-name datepicker)))
    (should (eq 'slack-text-message-composition-object
                (eieio-object-class-name (oref datepicker placeholder))))
    (should (equal "1990-04-28" (oref datepicker initial-date)))
    (should (equal "G=RRF" (oref datepicker action-id)))))

(ert-deftest slack-test-block-to-string ()
  ;; nil
  (should (eq nil (slack-block-to-string nil)))
  ;; slack-section-layout-block
  (let* ((input '(:type "section" :text (:type "plain_text" :text "Hello") :fields ((:type "plain_text" :text "Foo") (:type "plain_text" :text "Bar"))))
         (out (slack-block-to-string
               (slack-create-layout-block input))))
    (should (equal "Hello\nFoo\nBar" out)))
  ;; slack-divider-layout-block
  (let* ((lui-fill-column 10)
         (input '(:type "divider"))
         (out (slack-block-to-string
               (slack-create-layout-block input))))
    (should (eq 10 (length out)))
    (should (equal "----------" out)))
  ;; slack-image-layout-block
  (let* ((input '(:type "image" :block_id "DxZ" :image_url "https://goldengate.png" :alt_text "Example Image" :title (:type "plain_text" :text "Example Image" :emoji t) :fallback "600x800px image" :image_width 600 :image_height 800 :image_bytes 828593))
         (out (slack-block-to-string
               (slack-create-layout-block input))))
    (should (equal "Example Image (829 kB)\n[Image]" out)))
  ;; slack-actions-layout-block
  (let* ((input '(:type "actions" :elements ((:type "button" :text (:type "plain_text" :text "Button1") :action_id "Foo") (:type "button" :text (:type "plain_text" :text "Button2") :action_id "Bar"))))
         (actions (slack-create-layout-block input))
         (out (slack-block-to-string actions)))
    (should (equal "Button1 Button2" out)))
  ;; slack-context-layout-block
  (let* ((input '(:type "context" :block_id "CmiG" :elements ((:type "plain_text" :text "For more info, contact support@acme.inc" :emoji t) (:fallback "666x1000px image" :image_url "https://1YjNtFtJlMTaC26A/o.jpg" :image_width 666 :image_height 1000 :image_bytes 107304 :type "image" :alt_text "alt text for image"))))
         (context (slack-create-layout-block input))
         (out (slack-block-to-string context)))
    (should (equal "For more info, contact support@acme.inc [Image]" out)))
  ;; slack-static-select-block-element
  (let* ((input '(:type "static_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :initial_option (:text (:type "plain_text" :text "Choice 1" :emoji t) :value "value-0") :options ((:text (:type "plain_text" :text "Choice 1" :emoji t) :value "value-0") (:text (:type "plain_text" :text "Choice 2" :emoji t) :value "value-1") (:text (:type "plain_text" :text "Choice 3" :emoji t) :value "value-2")) :action_id "tIzc"))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal "Choice 1" out)))
  ;; slack-external-select-block-element
  (let* ((input '(:type "external_select" :placeholder (:type "plain_text" :text "Select an item" :emoji t) :action_id "T6JV"))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal "Select an item" out)))
  ;; slack-user-select-block-element
  (let* ((input '(:type "users_select" :action_id "4ji5" :initial_user "U0G2XCVQV" :placeholder (:type "plain_text" :text "Select a user" :emoji t)))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal "USER: U0G2XCVQV" out))
    (should (eq t (get-text-property 0 'slack-lazy-user-name out)))
    (should (equal "U0G2XCVQV" (get-text-property 0 'slack-user-id out))))
  ;; slack-conversation-select-block-element
  (let* ((input '(:type "conversations_select" :action_id "2/eZ" :initial_conversation "C0G31N06B" :placeholder (:type "plain_text" :text "Select a conversation" :emoji t)))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal "CONVERSATION: C0G31N06B" out))
    (should (eq t (get-text-property 0 'slack-lazy-conversation-name out)))
    (should (equal "C0G31N06B" (get-text-property 0 'slack-conversation-id out))))
  ;; slack-channel-select-block-element
  (let* ((input '(:type "channels_select" :action_id "CL5OA" :initial_channel "C0G31N06B" :placeholder (:type "plain_text" :text "Select a channel" :emoji t)))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal "CHANNEL: C0G31N06B" out))
    (should (eq t (get-text-property 0 'slack-lazy-conversation-name out)))
    (should (equal "C0G31N06B" (get-text-property 0 'slack-conversation-id out))))
  ;; slack-overflow-block-element
  (let* ((input '(:type "overflow" :options ((:text (:type "plain_text" :text "Option 1" :emoji t) :value "value-0") (:text (:type "plain_text" :text "Option 2" :emoji t) :value "value-1") (:text (:type "plain_text" :text "Option 3" :emoji t) :value "value-2") (:text (:type "plain_text" :text "Option 4" :emoji t) :value "value-3")) :action_id "3O/yl"))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal " … " out))
    )
  ;; slack-date-picker-block-element
  (let* ((input '(:type "datepicker" :initial_date "1990-04-28" :placeholder (:type "plain_text" :text "Select a date" :emoji t) :action_id "5/4"))
         (out (slack-block-to-string
               (slack-create-block-element input ""))))
    (should (equal "1990-04-28" out)))
  ;; slack-text-message-composition-object
  (let* ((input '(:type "plain_text" :text "hello\nworld" :emoji t :verbatim :json-false))
         (out (slack-block-to-string
               (slack-create-text-message-composition-object input))))
    (should (equal "hello\nworld" out))))

(ert-deftest slack-test-mrkdwn-regex-bold ()
  (let ((bold "aaa *Ace Wasabi Rock-n-Roll Sushi Bar* aaa"))
    (string-match slack-mrkdwn-regex-bold bold)
    (should (equal (match-string 3 bold)
                   "Ace Wasabi Rock-n-Roll Sushi Bar"))
    (should (eq (match-beginning 2) 4))
    (should (eq (match-beginning 4) 37)))
  (should (string-match-p slack-mrkdwn-regex-bold "*@channel お題案だしてもらったので以下から2つ選んでください*"))

  (should (not (string-match-p slack-mrkdwn-regex-bold "* bbb *")))
  (should (not (string-match-p slack-mrkdwn-regex-bold "*bbb*aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-bold "aaa*bbb*")))
  (should (not (string-match-p slack-mrkdwn-regex-bold "aaa*bbb*bbb")))
  (should (not (string-match-p slack-mrkdwn-regex-bold "aaa *Ace Wasabi Rock-n-Roll\n Sushi Bar* aaa")))
  (should (string-match-p slack-mrkdwn-regex-bold "*Ace Wasabi Rock-n-Roll Sushi Bar*")))

(ert-deftest slack-test-mrkdwn-regex-italic ()
  (let ((italic "aaa _Ace Wasabi Rock-n-Roll Sushi Bar_ aaa"))
    (string-match slack-mrkdwn-regex-italic italic)
    (should (equal (match-string 3 italic)
                   "Ace Wasabi Rock-n-Roll Sushi Bar"))
    (should (equal "_" (match-string 2 italic)))
    (should (equal "_" (match-string 4 italic)))
    (should (eq (match-beginning 2) 4))
    (should (eq (match-beginning 4) 37)))
  (should (not (string-match-p slack-mrkdwn-regex-italic "_bbb_aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-italic "aaa_bbb_")))
  (should (not (string-match-p slack-mrkdwn-regex-italic "aaa_bbb_aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-italic "SOME_ENV_BAR")))
  (should (not (string-match-p slack-mrkdwn-regex-italic "https://example.com/foo_bar_baz.html")))
  (should (not (string-match-p slack-mrkdwn-regex-italic "aaa _Ace Wasabi Rock-n-Roll \nSushi Bar_ aaa")))
  (should (string-match-p slack-mrkdwn-regex-italic "_a a a_"))
  (should (string-match-p slack-mrkdwn-regex-italic "_ aaa _"))
  (should (string-match-p slack-mrkdwn-regex-italic "_Ace Wasabi Rock-n-Roll Sushi Bar_")))

(ert-deftest slack-test-mrkdwn-regex-strike ()
  (let ((strike "aaa ~Ace Wasabi Rock-n-Roll Sushi Bar~ aaa"))
    (string-match slack-mrkdwn-regex-strike strike)
    (should (equal (match-string 3 strike)
                   "Ace Wasabi Rock-n-Roll Sushi Bar"))
    (should (eq (match-beginning 2) 4))
    (should (eq (match-beginning 4) 37)))
  (should (not (string-match-p slack-mrkdwn-regex-strike "~ bbb ~")))
  (should (not (string-match-p slack-mrkdwn-regex-strike "~bbb~aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-strike "aaa~bbb~")))
  (should (not (string-match-p slack-mrkdwn-regex-strike "aaa~bbb~aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-strike "aaa ~Ace Wasabi Rock-n-Roll\n Sushi Bar~ aaa")))
  (should (string-match-p slack-mrkdwn-regex-strike "~Ace Wasabi Rock-n-Roll Sushi Bar~"))
  (should (string-match-p slack-mrkdwn-regex-strike "~*When:*  7月 31, 2020 16:30 - 17:00~")))

(ert-deftest slack-test-mrkdwn-regex-code ()
  (let ((code "aaa `Ace Wasabi Rock-n-Roll Sushi Bar` aaa"))
    (should (string-match-p slack-mrkdwn-regex-code code))
    (string-match slack-mrkdwn-regex-code code)
    (should (equal "Ace Wasabi Rock-n-Roll Sushi Bar"
                   (match-string 3 code)))
    (should (eq (match-beginning 2) 4))
    (should (eq (match-beginning 4) 37))
    )
  ;; TODO
  ;; (let ((block "   ```This is a `code` block\nAnd it's multi-line```   "))
  ;;   (should (eq nil (string-match-p slack-mrkdwn-regex-code block))))
  (should (string-match-p slack-mrkdwn-regex-code "aaa`bbb`aaa"))
  (should (string-match-p slack-mrkdwn-regex-code "aaa`bbb`"))
  (should (not (string-match-p slack-mrkdwn-regex-code "   ```This is a code block\nAnd it's multi-line```   ")))
  (should (not (string-match-p slack-mrkdwn-regex-code "aaa `Ace Wasabi \nRock-n-Roll Sushi Bar` aaa")))
  (should (string-match-p slack-mrkdwn-regex-code "`bbb`aaa"))
  (should (string-match-p slack-mrkdwn-regex-code "` bbb `")))

(ert-deftest slack-test-mrkdwn-regex-code-block ()
  (let ((block "   ```This is a code block\nAnd it's multi-line```   "))
    (string-match slack-mrkdwn-regex-code-block block)
    (should (equal "This is a code block\nAnd it's multi-line"
                   (match-string 2 block)))
    (should (eq 3 (match-beginning 1)))
    (should (eq 46 (match-beginning 4))))
  (let ((block "   ```\nThis is a code block\nAnd it's multi-line\n```   "))
    (string-match slack-mrkdwn-regex-code-block block)
    (should (equal "This is a code block\nAnd it's multi-line"
                   (match-string 2 block)))
    (should (eq 3 (match-beginning 1)))
    (should (eq 47 (match-beginning 4))))
  (should (string-match-p slack-mrkdwn-regex-code-block "```\nbbb\naaa\n```\n"))
  (should (not (string-match-p slack-mrkdwn-regex-code-block "aaa```bbb```aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-code-block "aaa```bbb```")))
  (should (not (string-match-p slack-mrkdwn-regex-code-block "```bbb```aaa")))
  (should (not (string-match-p slack-mrkdwn-regex-code-block "aaa `Ace Wasabi Rock-n-Roll Sushi Bar` aaa"))))


(ert-deftest slack-test-mrkdwn-regex-blockquote ()
  (let ((blockquote " > aaa aaa"))
    (string-match slack-mrkdwn-regex-blockquote blockquote)
    (should (equal "aaa aaa"
                   (match-string 3 blockquote)))
    (should (eq 1 (match-beginning 1))))
  (should (string-match-p slack-mrkdwn-regex-blockquote ">aaa"))
  (should (string-match-p slack-mrkdwn-regex-blockquote " > aaa"))
  (should (string-match-p slack-mrkdwn-regex-blockquote " >aaa"))
  (should (not (string-match-p slack-mrkdwn-regex-blockquote ">")))
  (should (not (string-match-p slack-mrkdwn-regex-blockquote "a > a")))
  (should (not (string-match-p slack-mrkdwn-regex-blockquote "a >"))))

(ert-deftest slack-test-mrkdwn-regex-list ()
  (let ((str "- aaa\n- bbb"))
    (string-match slack-mrkdwn-regex-list str)
    (should (string= "-" (match-string 2 str)))
    (should (string= "aaa" (match-string 4 str))))
  (let ((str "* aaa\n* bbb"))
    (string-match slack-mrkdwn-regex-list str)
    (should (string= "*" (match-string 2 str)))
    (should (string= "aaa" (match-string 4 str))))
  (let ((str "  -  aaa"))
    (string-match slack-mrkdwn-regex-list str)
    (should (string= "-" (match-string 2 str)))
    (should (string= "  " (match-string 1 str)))
    (should (string= " aaa" (match-string 4 str))))
  (let ((str "  1. aaa\n  2.bbb"))
    (string-match slack-mrkdwn-regex-list str)
    (should (string= "1." (match-string 2 str)))
    (should (string= "  " (match-string 1 str)))
    (should (string= "aaa" (match-string 4 str)))))

(ert-deftest slack-test-rich-text-section ()
  (let ((payload (list :type "rich_text_section"
                       :elements (list (list :type "text" :text "Hello")
                                       (list :type "text" :text " World")))))
    (should (string= "Hello World" (slack-block-to-string
                                    (slack-create-rich-text-block-element payload))))))

(ert-deftest slack-test-rich-text-preformatted ()
  (let* ((payload (list :type "rich_text_preformatted"
                        :elements (list (list :type "text" :text "Hello")
                                        (list :type "text" :text " World"))))
         (text (slack-block-to-string
                (slack-create-rich-text-block-element payload))))
    (should (eq 'slack-mrkdwn-code-block-face
                (get-text-property 0 'face text)))))

(ert-deftest slack-test-rich-text-quote ()
  (let* ((payload (list :type "rich_text_quote"
                        :elements (list (list :type "text" :text "Hello\n")
                                        (list :type "text" :text "\nWorld"))))
         (text (slack-block-to-string (slack-create-rich-text-block-element payload))))
    (should (eq 'slack-mrkdwn-blockquote-face
                (get-text-property 0 'face text)))))

(ert-deftest slack-test-rich-text-list ()
  (let ((elements (list (list :type "rich_text_section"
                              :elements (list (list :type "text" :text "foo")))
                        (list :type "rich_text_section"
                              :elements (list (list :type "text" :text "bar")))
                        (list :type "rich_text_section"
                              :elements (list (list :type "text" :text "baz"))))))

    (let* ((payload (list :type "rich_text_list"
                          :elements elements
                          :style "bullet"
                          :indent 0))
           (text (slack-block-to-string (slack-create-rich-text-block-element payload))))
      (should (string= (concat (format "%s foo" slack-mrkdwn-list-bullet)
                               "\n"
                               (format "%s bar" slack-mrkdwn-list-bullet)
                               "\n"
                               (format "%s baz" slack-mrkdwn-list-bullet)
                               "\n")
                       text)))

    (let* ((payload (list :type "rich_text_list"
                          :elements elements
                          :style "bullet"
                          :indent 1))
           (text (slack-block-to-string (slack-create-rich-text-block-element payload))))
      (should (string= (concat (format "  %s foo" slack-mrkdwn-list-bullet)
                               "\n"
                               (format "  %s bar" slack-mrkdwn-list-bullet)
                               "\n"
                               (format "  %s baz" slack-mrkdwn-list-bullet)
                               "\n")
                       text)))

    (let* ((payload (list :type "rich_text_list"
                          :elements elements
                          :style "ordered"
                          :indent 0))
           (text (slack-block-to-string (slack-create-rich-text-block-element payload))))
      (should (string= (concat "1. foo"
                               "\n"
                               "2. bar"
                               "\n"
                               "3. baz"
                               "\n")
                       text)))

    (let* ((payload (list :type "rich_text_list"
                          :elements elements
                          :style "ordered"
                          :indent 1))
           (text (slack-block-to-string (slack-create-rich-text-block-element payload))))
      (should (string= (concat "  1. foo"
                               "\n"
                               "  2. bar"
                               "\n"
                               "  3. baz"
                               "\n")
                       text)))))

(ert-deftest slack-test-rich-text-list-offset ()
  ;; Slack continues the numbering of ordered lists split by nested
  ;; content through the list's "offset" attribute.
  (let* ((payload (list :type "rich_text"
                        :elements (list
                                   (list :type "rich_text_section"
                                         :elements (list (list :type "text" :text "intro\n")))
                                   (list :type "rich_text_list"
                                         :elements (list (list :type "rich_text_section"
                                                               :elements (list (list :type "text"
                                                                                     :text "first item"))))
                                         :style "ordered"
                                         :indent 0)
                                   (list :type "rich_text_list"
                                         :elements (list (list :type "rich_text_section"
                                                               :elements (list (list :type "text"
                                                                                     :text "sub one"))))
                                         :style "bullet"
                                         :indent 1)
                                   (list :type "rich_text_list"
                                         :elements (list (list :type "rich_text_section"
                                                               :elements (list (list :type "text"
                                                                                     :text "second item"))))
                                         :style "ordered"
                                         :indent 0
                                         :offset 1))))
         (block (slack-create-rich-text-block payload)))
    (should (string= (concat "intro\n"
                             "1. first item\n"
                             (format "  %s sub one\n" slack-mrkdwn-list-bullet)
                             "2. second item\n")
                     (slack-block-to-string block)))
    (should (string= (concat "intro\n"
                             "1. first item\n"
                             "  - sub one\n"
                             "2. second item\n")
                     (slack-block-to-mrkdwn block)))))

(ert-deftest slack-test-rich-text-text-element ()
  (let ((payload '(:type "text" :text "Hello this is rich text")))
    (should (string= "Hello this is rich text" (slack-block-to-string
                                                (slack-create-rich-text-element payload))))))

(ert-deftest slack-test-rich-text-element-style ()
  (let* ((bold '(:bold t))
         (italic '(:italic t))
         (strike '(:strike t))
         (code '(:code t))
         (payload '(:type "text" :text "Hello this is rich text")))
    (cl-labels ((get-face-property (style)
                                   (get-text-property 0
                                                      'face
                                                      (slack-block-to-string
                                                       (slack-create-rich-text-element
                                                        (plist-put payload :style style))))))
      (should (eq 'slack-mrkdwn-bold-face (get-face-property bold)))
      (should (eq 'slack-mrkdwn-italic-face (get-face-property italic)))
      (should (eq 'slack-mrkdwn-strike-face (get-face-property strike)))
      (should (eq 'slack-mrkdwn-code-face (get-face-property code))))))

(ert-deftest slack-test-rich-text-channel-element ()
  (slack-test-setup
    (let ((payload (list :type "channel" :channel_id channel-id)))
      (should (string= (format "#%s" channel-name)
                       (slack-block-to-string (slack-create-rich-text-element payload)
                                              (list :team team)))))))

(ert-deftest slack-test-rich-text-user-elemenmt ()
  (slack-test-setup
    (let ((payload (list :type "user" :user_id user-id)))
      (should (string= (format "@%s" display-name)
                       (slack-block-to-string (slack-create-rich-text-element payload)
                                              (list :team team)))))))

(ert-deftest slack-test-rich-text-emoji-element ()
  (let ((payload (list :type "emoji" :name "smile")))
    (should (string= ":smile:" (slack-block-to-string (slack-create-rich-text-element payload))))))

(ert-deftest slack-test-rich-text-link-element ()
  (let* ((url "https://www.gnu.org/software/emacs/")
         (payload (list :type "link" :url url :text nil)))
    (should (string= (format "<%s|%s>" url url)
                     (slack-block-to-string (slack-create-rich-text-element payload)))))
  (let* ((url "https://www.gnu.org/software/emacs/")
         (text "GNU Emacs - GNU Project")
         (payload (list :type "link" :url url :text text)))
    (should (string= (format "<%s|%s>" url text)
                     (slack-block-to-string (slack-create-rich-text-element payload))))))

(ert-deftest slack-test-rich-text-usergroup-element ()
  (slack-test-setup
    (let ((payload (list :type "usergroup" :usergroup_id usergroup-id)))
      (should (string= (format "@%s" usergroup-handle)
                       (slack-block-to-string (slack-create-rich-text-element payload)
                                              (list :team team)))))))

(ert-deftest slack-test-rich-text-date-element ()
  (let* ((time (current-time))
         (payload (list :type "date" :timestamp (format-time-string "%s") time)))
    (should (string= (format-time-string "%Y-%m-%d %H:%M:%S" time)
                     (slack-block-to-string (slack-create-rich-text-element payload))))))

(ert-deftest slack-test-rich-text-range-element ()
  (let ((payload (list :type "broadcast" :range "here")))
    (should (string= "@here" (slack-block-to-string (slack-create-rich-text-element payload))))))

(defun slack-test-parse-blocks (str)
  (let* ((json-object-type 'plist)
         (json-array-type 'list))
    (plist-get (json-read-from-string (json-encode
                                       (with-temp-buffer
                                         (insert str)
                                         (slack-create-blocks-from-buffer))))
               :blocks)))

(ert-deftest slack-test-create-blocks-from-buffer ()
  (let* ((str (string-trim "*<!here> fff*"))
         (blocks (slack-test-parse-blocks str)))
    (let ((block (car blocks)))
      (should (string= "rich_text" (plist-get block :type)))
      (let ((elements (plist-get block :elements)))
        (should (eq 1 (length elements)))
        (let ((section (car elements)))
          (let ((elements (plist-get section :elements)))
            (should (eq 1 (length elements)))
            (let ((element (car elements)))
              (should (string= "text" (plist-get element :type)))
              (should (string= "<!here> fff" (plist-get element :text)))
              (should (eq 2 (length (plist-get element :style))))
              (let ((style (plist-get element :style)))
                (should (eq t (plist-get style :bold))))))))))
;; (:type "mrkdwn" :text "*<!channel|channel> お題案だしてもらったので以下から2つ選んでください*
;; You may vote for multiple options" :verbatim t)
  (let* ((str (string-trim "
bold *bold* bold
italic _italic_ italic
strike ~strike~ strike
code `code` code
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (should (string= "rich_text" (plist-get block :type)))
      (should (eq 1 (length (plist-get block :elements))))
      (let ((section (car (plist-get block :elements))))
        (should (string= "rich_text_section" (plist-get section :type)))
        (let ((elements (plist-get section :elements)))
          (dolist (style  '(("bold" . :bold)
                            ("italic" . :italic)
                            ("strike" . :strike)
                            ("code" . :code)))
            (should (eq 1 (length (cl-remove-if #'(lambda (el) (or (null (plist-get el :style))
                                                                   (not (plist-get (plist-get el :style)
                                                                                   (cdr style)))))
                                                elements))))
            (should (string= (car style)
                             (plist-get (cl-find-if #'(lambda (el) (and (plist-get el :style)
                                                                        (plist-get (plist-get el :style)
                                                                                   (cdr style))))
                                                    elements)
                                        :text))))))))
  (let* ((str (string-trim "
```
*code*
block
:smile:
<@USERID>
https://google.com
```
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (let ((elements (plist-get block :elements)))
        (should (eq 1 (length elements)))
        (let ((section (car elements)))
          (should (string= "rich_text_preformatted" (plist-get section :type)))
          (let ((elements (plist-get section :elements)))
            (should (eq 1 (length elements)))
            (let ((element (car elements)))
              (should (string= "text" (plist-get element :type)))
              (should (string= "*code*\nblock\n:smile:\n<@USERID>\nhttps://google.com" (plist-get element :text)))))))))

  (let* ((str (string-trim "
> bold *bold* bold
> quote
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (let ((elements (plist-get block :elements)))
        (should (eq 1 (length elements)))
        (let ((section (car elements)))
          (should (string= "rich_text_quote" (plist-get section :type)))
          (let ((elements (plist-get section :elements)))
            (should (eq 3 (length elements)))
            (dolist (style '(("bold" . :bold)))
              (should (eq 1 (length (cl-remove-if #'(lambda (el) (or (null (plist-get el :style))
                                                                     (not (plist-get (plist-get el :style)
                                                                                     (cdr style)))))
                                                  elements))))
              (should (string= (car style)
                               (plist-get (cl-find-if #'(lambda (el) (and (plist-get el :style)
                                                                          (plist-get (plist-get el :style)
                                                                                     (cdr style))))
                                                      elements)
                                          :text)))))))))

  (let* ((str (string-trim "
1. list
2. *bold*
3. ordered
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (let ((elements (plist-get block :elements)))
        (should (eq 1 (length elements)))
        (let ((section (car elements)))
          (should (string= "rich_text_list" (plist-get section :type)))
          (should (string= "ordered" (plist-get section :style)))
          (should (eq 0 (plist-get section :indent)))
          (let ((elements (plist-get section :elements)))
            (should (eq 3 (length elements)))

            (let* ((bold-section (cadr elements))
                   (elements (plist-get bold-section :elements)))
              (dolist (style '(("bold" . :bold)))
                (should (eq 1 (length (cl-remove-if #'(lambda (el) (or (null (plist-get el :style))
                                                                       (not (plist-get (plist-get el :style)
                                                                                       (cdr style)))))
                                                    elements))))
                (should (string= (car style)
                                 (plist-get (cl-find-if #'(lambda (el) (and (plist-get el :style)
                                                                            (plist-get (plist-get el :style)
                                                                                       (cdr style))))
                                                        elements)
                                            :text))))))))))
  (let* ((str (string-trim "
 - list
 - `code`
 - bullet

"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (let ((elements (plist-get block :elements)))
        (should (eq 1 (length elements)))
        (let ((section (car elements)))
          (should (string= "rich_text_list" (plist-get section :type)))
          (should (string= "bullet" (plist-get section :style)))
          ;; One leading space is less than one two-space indentation level.
          (should (eq 0 (plist-get section :indent)))
          (let ((elements (plist-get section :elements)))
            (should (eq 3 (length elements)))

            (let* ((bold-section (cadr elements))
                   (elements (plist-get bold-section :elements)))
              (dolist (style '(("code" . :code)))
                (should (eq 1 (length (cl-remove-if #'(lambda (el) (or (null (plist-get el :style))
                                                                       (not (plist-get (plist-get el :style)
                                                                                       (cdr style)))))
                                                    elements))))
                (should (string= (car style)
                                 (plist-get (cl-find-if #'(lambda (el) (and (plist-get el :style)
                                                                            (plist-get (plist-get el :style)
                                                                                       (cdr style))))
                                                        elements)
                                            :text))))))))))
  (let* ((str (string-trim "
<@U0G2XCVQV>
<!channel>
<#C0G31N06B>
<!subteam^USLACKBOT>
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (should (string= "rich_text" (plist-get block :type)))
      (should (eq 1 (length (plist-get block :elements))))
      (let ((section (car (plist-get block :elements))))
        (should (string= "rich_text_section" (plist-get section :type)))
        (let ((elements (plist-get section :elements)))
          (dolist (mention  '(("U0G2XCVQV" . ("user" . :user_id))
                              ("channel" . ("broadcast" . :range))
                              ("C0G31N06B" . ("channel" . :channel_id))
                              ("USLACKBOT" . ("usergroup" . :usergroup_id))))
            (let ((type (cadr mention))
                  (v (car mention))
                  (key (cddr mention)))
              (should (eq 1 (length (cl-remove-if #'(lambda (el) (not (string= (plist-get el :type)
                                                                               type)))
                                                  elements))))
              (should (string= v
                               (plist-get (cl-find-if #'(lambda (el) (string= (plist-get el :type)
                                                                              type))
                                                      elements)
                                          key)))))))))
  (let* ((str (string-trim "
:dog2:
:man-biking:
:man_dancing:
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (should (string= "rich_text" (plist-get block :type)))
      (should (eq 1 (length (plist-get block :elements))))
      (let ((section (car (plist-get block :elements))))
        (should (not (null section)))
        (should (string= "rich_text_section" (plist-get section :type)))
        (let ((elements (plist-get section :elements)))
          (should (equal '("dog2" "man-biking" "man_dancing")
                         (mapcar #'(lambda (el) (plist-get el :name))
                                 (cl-remove-if #'(lambda (el) (not (string= "emoji" (plist-get el :type))))
                                               elements)))))
        )))
  (let* ((str (string-trim "
https://api.slack.com/changelog/2019-09-what-they-see-is-what-you-get-and-more-and-less
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (should (string= "rich_text" (plist-get block :type)))
      (should (eq 1 (length (plist-get block :elements))))
      (let ((section (car (plist-get block :elements))))
        (should (not (null section)))
        (should (string= "rich_text_section" (plist-get section :type)))
        (let ((elements (plist-get section :elements)))
          (should (eq 1 (length (cl-remove-if #'(lambda (el) (not (string= "link" (plist-get el :type))))
                                              elements))))
          (should (string= "https://api.slack.com/changelog/2019-09-what-they-see-is-what-you-get-and-more-and-less"
                           (plist-get (cl-find-if #'(lambda (el) (string= "link" (plist-get el :type)))
                                                  elements)
                                      :url))))))))

(ert-deftest slack-test-create-blocks-ordered-list-offset ()
  ;; Nested content between ordered items splits them into separate
  ;; rich_text_list blocks; the numbering the user typed is preserved
  ;; through the "offset" attribute, so Slack renders "2." and not a
  ;; second "1.".
  (let* ((str (string-trim "
intro line
1. first item
  - sub one
  - sub two
2. second item
trailing line
"))
         (blocks (slack-test-parse-blocks str)))
    (should (not (null blocks)))
    (let ((block (car blocks)))
      (should (string= "rich_text" (plist-get block :type)))
      (let ((elements (plist-get block :elements)))
        (should (eq 5 (length elements)))
        (let ((first-list (nth 1 elements))
              (sub-list (nth 2 elements))
              (second-list (nth 3 elements)))
          (should (string= "rich_text_list" (plist-get first-list :type)))
          (should (string= "ordered" (plist-get first-list :style)))
          (should (null (plist-get first-list :offset)))
          (should (string= "bullet" (plist-get sub-list :style)))
          (should (eq 1 (plist-get sub-list :indent)))
          (should (string= "rich_text_list" (plist-get second-list :type)))
          (should (string= "ordered" (plist-get second-list :style)))
          (should (eq 1 (plist-get second-list :offset))))))))

(ert-deftest slack-test-block-to-mrkdwn ()
  (let* ((payload (list :type "rich_text_preformatted"
                        :elements (list (list :type "text" :text "code\nblock"))))
         (block (slack-create-rich-text-block-element payload)))
    ;; Rich-text preformatted blocks use fenced Markdown with line breaks.
    (should (string= "```\ncode\nblock\n\n```\n"
                     (slack-block-to-mrkdwn block))))
  (let* ((payload (list :type "rich_text_quote"
                        :elements (list (list :type "text" :text "block\nquote"))))
         (block (slack-create-rich-text-block-element payload)))
    (should (string= "> block\n> quote\n"
                     (slack-block-to-mrkdwn block))))
  (let* ((payload (list :type "rich_text_list"
                        :style "bullet"
                        :indent 1
                        :elements (list (list :type "rich_text_section"
                                              :elements (list (list :type "text" :text "list")))
                                        (list :type "rich_text_section"
                                              :elements (list (list :type "text" :text "bullet"))))))
         (block (slack-create-rich-text-block-element payload)))
    (should (string= "  - list\n  - bullet\n"
                     (slack-block-to-mrkdwn block))))
  (let* ((payload (list :type "rich_text_list"
                        :style "ordered"
                        :indent 0
                        :elements (list (list :type "rich_text_section"
                                              :elements (list (list :type "text" :text "list")))
                                        (list :type "rich_text_section"
                                              :elements (list (list :type "text" :text "ordered"))))))
         (block (slack-create-rich-text-block-element payload)))
    (should (string= "1. list\n2. ordered\n"
                     (slack-block-to-mrkdwn block))))
  (let* ((payload (list :type "text" :text "text")))
    (dolist (s (list (cons (list :bold t) "*text*")
                     (cons (list :italic t) "_text_")
                     (cons (list :strike t) "~text~")
                     (cons (list :code t) "`text`")))
      (should (string= (cdr s)
                       (slack-block-to-mrkdwn
                        (slack-create-rich-text-element
                         (plist-put payload :style (car s))))))))
  (slack-test-setup
    (let* ((payload (list :type "channel" :channel_id channel-id))
           (block (slack-create-rich-text-element payload))
           (mrkdwn (slack-block-to-mrkdwn block (list :team team))))
      (should (string= (format "<#%s> " channel-id)
                       mrkdwn))
      (should (string= (format "#%s" channel-name)
                       (get-text-property 0 'display mrkdwn)))
      (should (eq 'slack-message-mention-face
                  (get-text-property 0 'face mrkdwn)))
      (should (not (null (get-text-property 0 'slack-mention-props mrkdwn))))
      (should (null (get-text-property 1 'slack-mention-props mrkdwn))))
    (let* ((payload (list :type "user" :user_id user-id))
           (block (slack-create-rich-text-element payload))
           (mrkdwn (slack-block-to-mrkdwn block (list :team team))))
      (should (string= (format "<@%s> " user-id)
                       mrkdwn))
      (should (string= (format "@%s" display-name)
                       (get-text-property 0 'display mrkdwn))))
    (let* ((payload (list :type "usergroup" :usergroup_id usergroup-id))
           (block (slack-create-rich-text-element payload))
           (mrkdwn (slack-block-to-mrkdwn block (list :team team))))
      (should (string= (format "<!subteam^%s> " usergroup-id)
                       mrkdwn))
      (should (string= (format "@%s" usergroup-handle)
                       (get-text-property 0 'display mrkdwn))))
    (let* ((payload (list :type "broadcast" :range "here"))
           (block (slack-create-rich-text-element payload))
           (mrkdwn (slack-block-to-mrkdwn block)))
      (should (string= "<!here> " mrkdwn))
      (should (string= "@here"
                       (get-text-property 0 'display mrkdwn))))
    (let* ((payload (list :type "emoji" :name "smile"))
           (block (slack-create-rich-text-element payload)))
      (should (string= ":smile:" (slack-block-to-mrkdwn block))))
    (let* ((payload (list :type "link" :url "https://google.com"))
           (block (slack-create-rich-text-element payload)))
      (should (string= "https://google.com"
                       (slack-block-to-mrkdwn block))))))
;;; message ranges (loaded blocks of history and the holes between them)

(defun slack-test-ts (n)
  "A realistic message timestamp for message number N.
Slack timestamps are fixed width, which is why the code can order them with
`string<'.  Do not shorten these to \"1\", \"7\", \"11\" in tests: as strings
\"11\" sorts before \"7\" and the tests would exercise an ordering that never
happens in practice."
  (format "16000000%02d.000000" n))

(defun slack-test-range (from to)
  (cons (slack-test-ts from) (slack-test-ts to)))

(ert-deftest slack-test-ranges-merge-overlapping ()
  (should (equal (list (slack-test-range 1 5))
                 (slack-ranges-add (list (slack-test-range 1 3))
                                   (slack-test-ts 2) (slack-test-ts 5))))
  ;; touching at a shared timestamp is still one block
  (should (equal (list (slack-test-range 1 5))
                 (slack-ranges-add (list (slack-test-range 1 3))
                                   (slack-test-ts 3) (slack-test-ts 5))))
  ;; a hole remains when the new block does not reach the old one
  (should (equal (list (slack-test-range 1 3) (slack-test-range 4 5))
                 (slack-ranges-add (list (slack-test-range 1 3))
                                   (slack-test-ts 4) (slack-test-ts 5))))
  ;; a block swallowed by an existing one changes nothing
  (should (equal (list (slack-test-range 1 5))
                 (slack-ranges-add (list (slack-test-range 1 5))
                                   (slack-test-ts 2) (slack-test-ts 3))))
  ;; arguments in the wrong order are tolerated
  (should (equal (list (slack-test-range 1 5))
                 (slack-ranges-add (list (slack-test-range 1 3))
                                   (slack-test-ts 5) (slack-test-ts 2)))))

(ert-deftest slack-test-ranges-add-joins-two-islands ()
  ;; the scenario that motivates all of this: loading the middle of a hole
  ;; makes the two islands one, so the load older/newer buttons disappear
  (let ((ranges (list (slack-test-range 3 3) (slack-test-range 7 7))))
    (setq ranges (slack-ranges-add ranges (slack-test-ts 2) (slack-test-ts 3)))
    (should (equal (list (slack-test-range 2 3) (slack-test-range 7 7)) ranges))
    (should (equal (list (cons (slack-test-ts 3) (slack-test-ts 7)))
                   (slack-ranges-gaps ranges)))
    (setq ranges (slack-ranges-add ranges (slack-test-ts 3) (slack-test-ts 7)))
    (should (equal (list (slack-test-range 2 7)) ranges))
    (should (null (slack-ranges-gaps ranges)))))

(ert-deftest slack-test-ranges-gaps ()
  (should (equal (list (cons (slack-test-ts 3) (slack-test-ts 7)))
                 (slack-ranges-gaps (list (slack-test-range 1 3)
                                          (slack-test-range 7 9)))))
  (should (equal (list (cons (slack-test-ts 3) (slack-test-ts 7))
                       (cons (slack-test-ts 9) (slack-test-ts 11)))
                 (slack-ranges-gaps (list (slack-test-range 1 3)
                                          (slack-test-range 7 9)
                                          (slack-test-range 11 12)))))
  (should (null (slack-ranges-gaps (list (slack-test-range 1 3)))))
  (should (null (slack-ranges-gaps nil))))

(ert-deftest slack-test-ranges-contain-p ()
  (let ((ranges (list (slack-test-range 1 3) (slack-test-range 7 9))))
    (should (slack-ranges-contain-p ranges (slack-test-ts 1)))
    (should (slack-ranges-contain-p ranges (slack-test-ts 2)))
    (should (slack-ranges-contain-p ranges (slack-test-ts 9)))
    (should (null (slack-ranges-contain-p ranges (slack-test-ts 5))))
    (should (null (slack-ranges-contain-p ranges (slack-test-ts 0))))))

(ert-deftest slack-test-ranges-clip ()
  ;; after trimming the store to the newest messages, blocks that are entirely
  ;; gone disappear and the surviving one starts where the store now starts
  (should (equal (list (slack-test-range 5 9))
                 (slack-ranges-clip (list (slack-test-range 1 3)
                                          (slack-test-range 4 9))
                                    (slack-test-ts 5))))
  (should (equal (list (slack-test-range 4 9))
                 (slack-ranges-clip (list (slack-test-range 1 3)
                                          (slack-test-range 4 9))
                                    (slack-test-ts 4))))
  (should (null (slack-ranges-clip (list (slack-test-range 1 3))
                                   (slack-test-ts 5))))
  (should (null (slack-ranges-clip (list (slack-test-range 1 3)) nil))))

(ert-deftest slack-test-room-record-fetched-range ()
  (let ((room (make-instance 'slack-channel :id "C1" :name "test")))
    (slack-room-record-fetched-range room nil
                                     :oldest (slack-test-ts 3)
                                     :latest (slack-test-ts 5))
    (should (equal (list (slack-test-range 3 5)) (slack-room-ranges room)))
    (should (null (oref room history-start-reached)))
    ;; an exhausted window with nothing in it still closes the hole, because
    ;; the bounds we asked for are now known to be fully loaded
    (slack-room-record-fetched-range room nil
                                     :oldest (slack-test-ts 5)
                                     :latest (slack-test-ts 8))
    (should (equal (list (slack-test-range 3 8)) (slack-room-ranges room)))
    (slack-room-record-fetched-range room nil
                                     :oldest (slack-test-ts 1)
                                     :latest (slack-test-ts 2)
                                     :reached-start t)
    (should (equal (list (slack-test-range 1 2) (slack-test-range 3 8))
                   (slack-room-ranges room)))
    (should (oref room history-start-reached))))

(ert-deftest slack-test-room-ranges-fallback ()
  ;; a room filled in before ranges were tracked behaves as it always did:
  ;; everything it holds counts as one contiguous block, so no holes are drawn
  (let ((room (make-instance 'slack-channel :id "C1" :name "test")))
    (oset room message-ids (list (slack-test-ts 1)
                                 (slack-test-ts 2)
                                 (slack-test-ts 3)))
    (should (equal (list (slack-test-range 1 3)) (slack-room-ranges room)))
    (should (null (slack-room-gaps room)))))

(ert-deftest slack-test-room-ensure-ranges-freezes-fallback ()
  ;; jumping to an old message in a room whose ranges were never recorded: the
  ;; guess about what we already had has to be taken BEFORE the old messages
  ;; are stored, or the room would look contiguous and the hole would vanish
  (let ((room (make-instance 'slack-channel :id "C1" :name "test")))
    (oset room message-ids (list (slack-test-ts 8) (slack-test-ts 9)))
    (slack-room-ensure-ranges room)
    ;; the jump brings in messages 1 and 2, far away from 8 and 9
    (oset room message-ids (list (slack-test-ts 1) (slack-test-ts 2)
                                 (slack-test-ts 8) (slack-test-ts 9)))
    (slack-room-add-range room (slack-test-ts 1) (slack-test-ts 2))
    (should (equal (list (slack-test-range 1 2) (slack-test-range 8 9))
                   (slack-room-ranges room)))
    (should (equal (list (slack-test-range 2 8)) (slack-room-gaps room)))))

(ert-deftest slack-test-room-trim-messages-clips-ranges ()
  ;; dropping old messages must also drop the promise that we have them
  (let ((room (make-instance 'slack-channel :id "C1" :name "test")))
    (dolist (n '(1 2 3))
      (puthash (slack-test-ts n)
               (make-instance 'slack-message :ts (slack-test-ts n))
               (oref room messages))
      (oset room message-ids (append (oref room message-ids)
                                     (list (slack-test-ts n)))))
    (slack-room-add-range room (slack-test-ts 1) (slack-test-ts 3))
    (oset room history-start-reached t)
    (slack-room-trim-messages room 2)
    (should (equal (list (slack-test-range 2 3)) (slack-room-ranges room)))
    (should (null (oref room history-start-reached)))))

;;; opening a message that is not loaded yet

(defun slack-test-message (team room ts text &optional thread-ts)
  (slack-message-create (append (list :type "message" :ts ts :user "U11111"
                                      :text text)
                                (and thread-ts (list :thread_ts thread-ts)))
                        team room))

(defmacro slack-test-with-registered-team (bindings &rest body)
  "Run BODY with a team and channel registered in the global team tables.
BINDINGS is (TEAM-VAR CHANNEL-VAR).  Slack looks teams up by id through those
tables, so a buffer cannot be built without them; they are cleaned up
afterwards, together with any buffer BODY created."
  (declare (indent 1) (debug t))
  (let ((team (car bindings))
        (channel (cadr bindings)))
    `(let* ((,team (make-instance 'slack-team
                                  :self-id "U00000" :id "T99999"
                                  :token "xoxb-token-for-tests"
                                  :name "test-team"))
            (,channel (make-instance 'slack-channel :id "C99999" :name "chan"))
            (before (buffer-list)))
       (puthash (oref ,team token) ,team slack-teams-by-token)
       (puthash (oref ,team id) (oref ,team token) slack-tokens-by-id)
       (puthash (oref ,channel id) ,channel (oref ,team channels))
       (puthash "U11111" (list :name "tester" :id "U11111"
                               :profile (list :display_name_normalized "Tester"
                                              :real_name_normalized "Tester"))
                (oref ,team users))
       (unwind-protect
           (progn ,@body)
         (remhash (oref ,team token) slack-teams-by-token)
         (remhash (oref ,team id) slack-tokens-by-id)
         (dolist (buf (buffer-list))
           (unless (memq buf before)
             (kill-buffer buf)))))))

(ert-deftest slack-test-open-message-loads-missing-thread ()
  "Opening a reply whose thread root is not loaded shows the whole thread.
Thread replies are not part of channel history, so loading messages around the
reply cannot help: the thread has to be fetched with conversations.replies.
The root also has to be told which messages are its replies, otherwise the
thread buffer draws the root and the separator and nothing else."
  (slack-test-with-registered-team (team channel)
    (let* ((root-ts (slack-test-ts 1))
           (reply-ts (slack-test-ts 3))
           (browsed nil)
           (history-calls 0)
           (replies-ts nil)
           ;; make the buffer current without touching windows, so that the
           ;; jump to the linked message happens where it would in real use
           (slack-buffer-function #'set-buffer)
           (slack-open-message-with-browser t))
      ;; the room holds recent history only; the thread root is older than all
      ;; of it, so `slack-open-message' cannot find it
      (slack-room-set-messages channel
                               (list (slack-test-message team channel
                                                         (slack-test-ts 8)
                                                         "recent"))
                               team)
      (slack-room-add-range channel (slack-test-ts 8) (slack-test-ts 8))
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq browsed url)))
                ((symbol-function 'slack-conversations-history)
                 (lambda (&rest _) (cl-incf history-calls)))
                ((symbol-function 'slack-conversations-replies)
                 (lambda (room ts team &rest args)
                   (setq replies-ts ts)
                   ;; the real endpoint answers with the root first
                   (funcall (plist-get args :after-success)
                            (list (slack-test-message team room root-ts
                                                      "the root" root-ts)
                                  (slack-test-message team room reply-ts
                                                      "the reply" root-ts))
                            nil nil))))
        (save-current-buffer
          (slack-open-message team channel reply-ts root-ts)))
      (should (equal root-ts replies-ts))
      ;; channel history says nothing about replies, so asking for it is waste
      (should (equal 0 history-calls))
      (slack-if-let* ((buffer (slack-buffer-find 'slack-thread-message-buffer
                                                 team channel root-ts)))
          (with-current-buffer (slack-buffer-buffer buffer)
            (should (string-match-p "the root" (buffer-string)))
            (should (string-match-p "the reply" (buffer-string)))
            ;; the permalink points at the reply, not at the root
            (should (equal reply-ts (slack-get-ts))))
        (ert-fail "no thread buffer was created"))
      (should (null browsed)))))

;;; lazy user fetch (`slack-user--maybe-fetch')
(ert-deftest slack-test-user-maybe-fetch-requests-uncached-user ()
  "Looking up an uncached user ID triggers one `slack-user-info-request'."
  (slack-test-setup
    (oset team token "xoxb-test-token")
    (let ((fetch-args nil))
      (cl-letf (((symbol-function 'slack-user-info-request)
                 (lambda (user-id team &key after-success)
                   (push (list user-id after-success) fetch-args)
                   ;; simulate a successful fetch: store the user, run callback
                   (puthash "U99999"
                            (list :id "U99999"
                                  :profile (list :display_name_normalized "External"
                                                 :real_name_normalized "External"))
                            (oref team users))
                   (when (functionp after-success)
                     (funcall after-success)))))
        (should (null (slack-user--find "U99999" team)))
        (should (equal 1 (length fetch-args)))
        ;; after the fetch callback, the user is cached
        (should (equal "External" (slack-user-name "U99999" team)))))))

(ert-deftest slack-test-user-maybe-fetch-does-not-duplicate ()
  "Repeated lookups for the same uncached user only fetch once."
  (slack-test-setup
    (oset team token "xoxb-test-token")
    (let ((fetch-count 0))
      (cl-letf (((symbol-function 'slack-user-info-request)
                 (lambda (&rest _)
                   (cl-incf fetch-count)
                   ;; do nothing; leave user uncached so the pending guard
                   ;; is what prevents the second call
                   nil)))
        (slack-user--find "U99999" team)
        (slack-user--find "U99999" team)
        (should (equal 1 fetch-count))))))

(ert-deftest slack-test-user-maybe-fetch-skips-bots ()
  "Bot IDs (starting with B) are not fetched via `users.info'."
  (slack-test-setup
    (oset team token "xoxb-test-token")
    (let ((fetch-count 0))
      (cl-letf (((symbol-function 'slack-user-info-request)
                 (lambda (&rest _) (cl-incf fetch-count))))
        (slack-user--find "B12345" team)
        (should (equal 0 fetch-count))))))

(ert-deftest slack-test-user-maybe-fetch-cached-user-no-fetch ()
  "A cached user is returned without triggering a fetch."
  (slack-test-setup
    (let ((fetch-count 0))
      (cl-letf (((symbol-function 'slack-user-info-request)
                 (lambda (&rest _) (cl-incf fetch-count))))
        (should (eq (car (gethash "U11111" (oref team users)))
                    (car (slack-user--find "U11111" team))))
        (should (equal 0 fetch-count))))))

(ert-deftest slack-test-user-maybe-fetch-no-token-no-fetch ()
  "When the team has no token, no fetch is attempted."
  (slack-test-setup
    (let ((fetch-count 0))
      (cl-letf (((symbol-function 'slack-user-info-request)
                 (lambda (&rest _) (cl-incf fetch-count))))
        (slack-user--find "U99999" team)
        (should (equal 0 fetch-count))))))

(ert-deftest slack-test-user-timezone-missing-tz-offset ()
  "Timezone functions return nil for users without `tz_offset'
\(e.g. external/Slack-Connect users) instead of crashing."
  (let ((external-user (list :id "UEXT01"
                             :profile (list :display_name_normalized "External"
                                            :real_name_normalized "External")))
        (nil-user nil))
    (should (null (slack-user-timezone external-user)))
    (should (null (slack-user-local-time external-user)))
    (should (null (slack-user-timezone nil-user)))
    (should (null (slack-user-local-time nil-user)))))

(ert-deftest slack-test-user-hidden-p-missing-deleted-field ()
  "Users without a `:deleted' field are not hidden.  External/Slack-Connect
users fetched via `users.info' often lack this field."
  (should (null (slack-user-hidden-p (list :id "UEXT01" :profile nil))))
  (should (null (slack-user-hidden-p (list :id "U11111" :deleted :json-false))))
  (should (slack-user-hidden-p (list :id "U11111" :deleted t))))

;;; reply-broadcast message user-ids scans text for mentions
(ert-deftest slack-test-reply-broadcast-user-ids-includes-mentions ()
  "A reply-broadcast message collects mentioned user IDs from text, not
just the sender."
  (slack-test-setup
   (let ((msg (slack-message-create
               (list :type "message"
                     :subtype "thread_broadcast"
                     :ts (slack-test-ts 5)
                     :user "U11111"
                     :text "Hey <@U22222> and <@U33333>")
               team
               channel)))
     (should (eq 'slack-reply-broadcast-message
                 (eieio-object-class-name msg)))
     (let ((ids (slack-message-user-ids msg)))
       (should (member "U11111" ids))
       (should (member "U22222" ids))
       (should (member "U33333" ids))))))

(defvar slack-tests-to-run
  (list
   "slack-export-test.el"
   "slack-modeline-test.el"
   "slack-vip-test.el"
   "slack-activity-feed-test.el"
   "slack-group-test.el"
   "slack-file-attach-test.el"
   "slack-image-test.el"
   "slack-permalink-test.el"
   "slack-channel-test.el"
   "slack-org-link-test.el"
   "slack-org-alert-test.el"
   "slack-message-test.el"
   "slack-stars-test.el"
   "slack-emoji-test.el"
   "slack-room-buffer-test.el"
   "slack-thread-message-buffer-test.el"
   "slack-read-mark-test.el"))

(dolist (test-file slack-tests-to-run)
  (load (expand-file-name
         test-file
         (file-name-directory (or load-file-name buffer-file-name)))
        nil
        nil))

(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))

