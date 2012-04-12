Zotonic Deployment with Github
==============================
This Zotonic module provides site deployment capabilities with Git.

It integrates with Post-Receive Hooks to automatically pull down and deploy
development changes pushed to Github.

By default Post-Receive Hooks are received at `/github-post-receive`, but that
is configurable in dispatch.

This module requires you to have your site in a Git repository. It doesn't
have to be on Github, but it's post-receive hook must POST to your Zotonic
server and tolerate HTTP 400 as a response.

The mod_github_sync post_receive_ips key in Config is used to control which
IPs are allowed to POST.

Configuration Properties
------------------------
mod_github_sync.accepted_ips a list of IP addresses separated however you like
mod_github_sync.accepted_token the single token that must be in the URL like
http://example.com/github-post-receive/$token

Future
------
Some kind soul will be so kind as to implement a replacement for this using
Zotonic API and use OAuth to provide better assurance of a valid caller :)

Provide periodic content backups by dumping database content to SQL,
committing it and other uploaded files and pushing the result to Github.