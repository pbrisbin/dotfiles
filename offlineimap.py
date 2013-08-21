import os
import re

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

def get_password(account):
    # should except on unset value
    return os.environ[account.upper() + "_PASSWORD"]
