<p>
  <a href="https://melpa.org/#/slack"><img alt="MELPA" src="https://melpa.org/packages/slack-badge.svg"/></a>
</p>
<p align="center"><img src="https://raw.githubusercontent.com/yuya373/emacs-slack/assets/assets/slack-logo.svg?sanitize=true" width=300 height=126/></p>
<p align="center"><b>Emacs Slack</b></p>
<p align="center">GNU Emacs client for <a href="https://slack.com/">Slack</a>.</p>

---

## Preview

You can see some gifs on the [wiki](https://github.com/emacs-slack/emacs-slack/wiki/ScreenShots).

## Configuration

[How to get token and cookie](#how-to-get-token-and-cookie)

If your token expires, you can use `slack-refresh-token` for a way to refresh interactively.

```elisp
(use-package slack
  :bind (("C-c S K" . slack-stop)
         ("C-c S c" . slack-select-rooms)
         ("C-c S u" . slack-select-unread-rooms)
         ("C-c S U" . slack-user-select)
         ("C-c S s" . slack-search-from-messages)
         ("C-c S J" . slack-jump-to-browser)
         ("C-c S j" . slack-jump-to-app)
         ("C-c S e" . slack-insert-emoji)
         ("C-c S E" . slack-message-edit)
         ("C-c S r" . slack-message-add-reaction)
         ("C-c S t" . slack-thread-show-or-create)
         ("C-c S g" . slack-message-redisplay)
         ("C-c S G" . slack-conversations-list-update-quick)
         ("C-c S q" . slack-quote-and-reply)
         ("C-c S Q" . slack-quote-and-reply-with-link)
         (:map slack-mode-map
               (("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-thread-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)
                ("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)))
         (:map slack-message-compose-buffer-mode-map
               (("C-c '" . slack-message-send-from-buffer)))
         )
  :custom
  (slack-extra-subscribed-channels (mapcar 'intern (list "some-channel")))
  :config
  (slack-register-team
     :name "clojurians"
     :token "xoxc-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
     :cookie "xoxd-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj; d-s=888888888888; lc=888888888888"
     :full-and-display-names t
     :default t
     :subscribed-channels nil ;; using slack-extra-subscribed-channels because I can change it dynamically
     ))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

```

## How to get token and cookie

Use the interactive command `slack-refresh-token` because it has the
most complete instructions to get token and cookies required for emacs-slack to work.

For further explanation, see the documentation for the emojme project:
[(github.com/jackellenberger/emojme)](https://github.com/jackellenberger/emojme#slack-for-web)

Note that it is only possible to obtain the cookie manually, not through
client-side javascript, due to it being set as `HttpOnly` and `Secure`. See
[OWASP HttpOnly](https://owasp.org/www-community/HttpOnly#Browsers_Supporting_HttpOnly).

## How to secure your token

If someone steals your token they can use the token to impersonate
you, reading and posting to Slack as if they were you.  It's important
to take reasonable precautions to secure your token.

One way to do this is by using the Emacs `auth-source` library. Read
the [auth-source
documentation](https://www.gnu.org/software/emacs/manual/html_node/auth/index.html)
to learn how to use it to store login information for remote services.

Then configure the `auth-sources` variable to select a "backend"
store. The default backend is `~/.authinfo` file, which is simple but
also un-encrypted. A more complex option is to encrypt that
`.~/authinfo` file with `gnupg` and configure `auth-sources` to use
`~/.authinfo.gpg` as the source for all passwords and secrets. Other
backends exist beyond these; read the documentation for details.

How to store your slack tokens in your `auth-source` backend will vary
depending which backend you chose. See documentation for details. The
"host" and "user" fields can be whatever you like as long as they are
unique; as a suggestion use "myslackteam.slack.com" for host, and use
your email address for user. The "secret" or "password" field should
contain the token you obtained earlier ([How to get
token and cookie](#how-to-get-token-and-cookie)).

Do the same for the cookie, however for the "user" field append `^cookie`, so if
for the token you picked `user@email.com` then for the cookie use
`user@email.com^cookie`.

Then finally, in your Emacs init read the token from your
`auth-source`:

``` elisp
(slack-register-team
 :name "myslackteam"
 :token (auth-source-pick-first-password
         :host "myslackteam.slack.com"
         :user "me@example.com")
 :subscribed-channels '((channel1 channel2)))
```

If your token starts with `xoxc` you'll also need to manually obtain the cookie
as described in [How to get token and cookie](#how-to-get-token-and-cookie) and
make sure the "user" has `^cookie` in it as described above in [How to secure
your token](#how-to-secure-your-token):

``` elisp
(slack-register-team
 :name "myslackteam"
 :token (auth-source-pick-first-password
         :host "myslackteam.slack.com"
         :user "me@example.com")
 :cookie (auth-source-pick-first-password
         :host "myslackteam.slack.com"
         :user "me@example.com^cookie")
 :subscribed-channels '((channel1 channel2)))
```

If you do not specify `:cookie` then you'll automatically be prompted for one if
you are using an `xoxc` token.

## How to use

I recommend to chat with slackbot for tutorial using `slack-im-select`.

Some terminology in the `slack-` functions:
- `im`: An IM (instant message) is a direct message between you and exactly one other Slack user.
- `channel`: A channel is a Slack channel which you are a member of
- `group`. Any chat (direct message or channel) which isn't an IM is a group.

- `slack-register-team`
  - set team configuration and create team.
  - :name and :token are required
- `slack-change-current-team`
  - change `slack-current-team` var
- `slack-start`
  - do authorize and initialize
- `slack-ws-close`
  - turn off websocket connection
- `slack-group-select`
  - select group from list
- `slack-im-select`
  - select direct message from list
- `slack-channel-select`
  - select channel from list
- `slack-group-list-update`
  - update group list
- `slack-im-list-update`
  - update direct message list
- `slack-channel-list-update`
  - update channel list
- `slack-message-embed-mention`
  - use to mention to user
- `slack-message-embed-channel`
  - use to mention to channel
- `slack-emoji-dwim`
  - select an emoji and use it where the cursor is: as a reaction to the
    message at point, or inserted in the text you are typing
- `slack-file-upload`
  - attach a file to the current message draft; it uploads when you send
- thread sync suggestion: when a thread exceeds
  `slack-thread-suggest-sync-threshold` messages (default 20) and the
  recent ones alternate between two or three people, the thread buffer
  shows a one-time hint that a quick sync might resolve things faster;
  turn it off with `slack-thread-suggest-sync`

### Tip

If your Slack team has a huge number of public channels, you may find
you hit your token rate limit. If that happens, set `(setq
slack-quick-update t)`: it will avoid the hit of rate limiting at
expense of functionality (like you will not have all public channel
available and search will not display room names when it doesn't know
about them).

## Notification

See [alert](https://github.com/jwiegley/alert).

## Org mode integration

`slack-org-link.el` provides the `emacs-slack:` Org link type, backed by
Slack permalinks:

```elisp
(require 'slack-org-link)
```

- `org-store-link` in a message, thread, search result or pinned item buffer
  stores an `[[emacs-slack:https://myteam.slack.com/archives/...][...]]` link
- following the link opens the message in emacs-slack and, when the team or
  room is not available, falls back to the system browser
  (`slack-org-open-in-browser-fallback`)
- exporting to HTML/Markdown emits the permalink as a real `https` link
- `slack-org-link-at-point` returns the link for the message at point, for
  use in `org-capture-templates`
- the `TEAMID|ROOMID|ts:TS` links written by the
  [ol-emacs-slack](https://github.com/ag91/ol-emacs-slack) package still
  work; the old `ol/...` function names are kept as obsolete aliases

`slack-org-alert.el` (optional) captures message alerts as Org TODO headings
so you can manage messages from the Org agenda. It is disabled until you
set the target file:

```elisp
(require 'slack-org-alert)
(setq slack-org-alert-file "~/agenda/Slack.org")
(slack-org-alert-setup)
(add-to-org-agenda-files "~/agenda/Slack.org")
```

Duplicate alerts are skipped (one heading per message), and
`slack-org-agenda-mark-all-done` marks every entry tagged with
`slack-org-alert-tag` (default `slack`) done in an agenda buffer. See
`slack-org-alert-heading-format`, `slack-org-alert-tag` and
`slack-org-alert-file` for customization.

## Extensions

- [helm-slack](https://github.com/yuya373/helm-slack)
- [ol-emacs-slack](https://github.com/ag91/ol-emacs-slack)
  (superseded: the Org link support now lives here, see
  "Org mode integration" above)

## How to debug

Please set `(setq slack-log-level 'debug)` to see useful messages that
may help pin point your issue. If you plan to add that to an issue,
make sure to edit out credentials. 

You can also use `(setq slack-log-level 'trace)`, but that only if you
need it because it will overwhelm your minibuffer with messages.

## Contributing

Please open an issue or better pull request for things that trouble you. 
## Dependencies

- [Alert](https://github.com/jwiegley/alert)
- [circe](https://github.com/jorgenschaefer/circe) (for the Linewise User
  Interface library).
- [Emojify](https://github.com/iqbalansari/emacs-emojify) (optional)
- [Oauth2](https://github.com/emacsmirror/oauth2/blob/master/oauth2.el)
  - do `package install`
- [request](https://github.com/tkf/emacs-request)
- [websocket](https://github.com/ahyatt/emacs-websocket)

## Similar projects

- https://github.com/wee-slack/wee-slack
