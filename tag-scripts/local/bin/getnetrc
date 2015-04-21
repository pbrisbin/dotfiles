#!/usr/bin/env python
#
# pbrisbin 2013 - retreive passwords by email address from ~/.netrc.
# Unlike typical netrc usage, this allows you to store credentials under
# the domain of the email address, rather than the hostname of the IMAP
# server (which may be the same for multiple sets of credentials).
#
# Example ~/.netrc:
#
#   machine gmail.com
#     login pbrisbin@gmail.com
#     password supersecret
#
#   machine codeclimate.com
#     login pat@codeclimate.com
#     password othersecret
#
# Usage:
#
#   $ getnetrc pbrisbin@gmail.com
#   supersecret
#
#   $ getnetrc pat@codeclimate.com
#   othersecret
#
###
import netrc
import sys

def get_password(email_address):
    try:
        net_rc = netrc.netrc()
    except IOError:
        return None

    try:
        domain = email_address.split('@', 2)[1]
    except:
        return None

    for host in net_rc.hosts.keys():
        if host == domain:
            login, _, password = net_rc.authenticators(host)

            if login == email_address:
                return password

    return None

if __name__ == '__main__':
    pw = get_password(sys.argv[1])

    if pw: print(pw)
