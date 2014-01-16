import os
import re
import netrc

mapping = { 'INBOX':              'INBOX'
          , '[Gmail]/All Mail':   'all_mail'
          , '[Gmail]/Drafts':     'drafts'
          , '[Gmail]/Important':  'important'
          , '[Gmail]/Sent Mail':  'sent_mail'
          , '[Gmail]/Spam':       'spam'
          , '[Gmail]/Starred':    'starred'
          , '[Gmail]/Trash':      'trash'
          }

r_mapping = { val: key for key, val in mapping.items() }

def nt_remote(folder):
    return mapping.get(folder, folder)

def nt_local(folder):
    return r_mapping.get(folder, folder)

# folderfilter = exclude(['Label', 'Label', ... ])
def exclude(excludes):
    return lambda folder: not folder in excludes

def get_password(email_address):
  net_rc = netrc.netrc()
  email_host = email_address.split('@', 2)[1]

  for host in net_rc.hosts.keys():
    if host == email_host:
      authenticator = net_rc.authenticators(host)

      if authenticator[0] == email_address:
        return authenticator[2]

  return None
