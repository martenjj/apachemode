;;; apache-mode.el --- major mode for Apache configuration files   -*- datetrack-update:nil -*-

;; Keywords:	languages, faces
;; Author:	Jonathan Marten  <jonathan.marten@uk.sun.com> or
;; Last edit:	12-May-2002      <rendhalver@xemacs.org>

;; This file is an add-on for XEmacs or GNU Emacs.
;;
;; It is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; It is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your copy of Emacs; see the file COPYING.  If not, write
;; to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; There isn't really much to say.  The list of keywords was derived from
;; the documentation for Apache 1.3; there may be some errors or omissions.
;;
;; There are currently no local keybindings defined, but the hooks are
;; there in the event that anyone gets around to adding any.
;;

;; PB: hopefully this will no longer be needed :)
;; no longer need to hack auto-mode-alist :)

;; To enable automatic selection of this mode when appropriate files are
;; visited, add the following to your favourite site or personal Emacs
;; configuration file:
;;
;;   (autoload 'apache-mode "apache-mode" "autoloaded" t)
;;   (add-to-list 'auto-mode-alist '("\\.htaccess$"   . apache-mode))
;;   (add-to-list 'auto-mode-alist '("httpd\\.conf$"  . apache-mode))
;;   (add-to-list 'auto-mode-alist '("srm\\.conf$"    . apache-mode))
;;   (add-to-list 'auto-mode-alist '("access\\.conf$" . apache-mode))
;;

;;; Change Log:
;;
;; Version 1.3, May 2002        updated keywords to include new directives in
;;                              Apache 2
;; Version 1.2.1, May 2002      separated apache directives into sections
;;                              for easier updating
;; Version 1.2, April 2002      Added mod_ssl 2.8.8, Apache-SSL 1.47 and
;;                              mod_perl 1.26 keywords.
;; Version 1.1, April 2002      changed var's to use customise
;;                              updated the keywords to apache 1.3.24
;;                              added apache-file-patterns to save having to hack
;;                              auto-mode-alist, added autoloaded function to add
;;                              apache-file-patterns to auto-mode-alist on autoload
;;
;; Version 1.0, October 1999	First public release


;;; Code:

;; Requires
(require 'font-lock)
(require 'regexp-opt)
(require 'custom)

;; Variables

;;;###autoload
(defgroup apache nil
  "Major mode for editing Apache configuration files."
  :prefix "apache-"
  :group 'languages)

(defcustom apache-manual-url "http://httpd.apache.org/"
  "*URL at which to find the Apache manual."
  :type 'string
  :group 'apache)

;;;###autoload
(defcustom apache-file-patterns
  (list "\\.htaccess\\(\\.default\\)?$" "httpd\\.conf\\(\\.default\\)?$"
	"srm\\.conf\\(\\.default\\)?$" "access\\.conf\\(\\.default\\)?$")
  "*List of file patterns for which to automatically invoke `apache-mode'."
  :type '(repeat (regexp :tag "Pattern"))
  :group 'apache)

(defcustom apache-mode-hook nil
  "*List of hook functions run by `apache-mode' (see `run-hooks')."
  :type 'hook
  :group 'apache)

(defvar apache-mode-map nil
  "Keymap used in `apache-mode' buffers.")

(defvar apache-mode-syntax-table nil
  "Syntax table for `apache-mode'.")

;; Font lock
(defconst apache-font-lock-keywords
  (purecopy
   (list
    (list "^\\s-*#.*$" 0 'font-lock-comment-face t)

    (list (concat                                       ; sections
	   "^\\s-*</?\\("
           (regexp-opt
            '(
              "Directory"                               ; core
              "DirectoryMatch"
              "Files"
              "FilesMatch"
              "IfDefine"
              "IfModule"
              "Limit"
              "LimitExcept"
              "Location"
              "LocationMatch"
              "VirtualHost"
              "Perl"                                    ; mod_perl
              ))
           "\\)\\(>\\|\\s-\\)")
	  1 'font-lock-function-name-face)

    (list (concat                                       ; keywords
	   "^\\s-*\\("
           (regexp-opt
            (delete-duplicates
             '(
	       "AcceptFilter"                           ; core
               "AcceptMutex"
               "AccessConfig"
               "AccessFileName"
               "AddDefaultCharset"
	       "AddModule"
               "AllowOverride"
               "AuthName"
               "AuthType"
               "BindAddress"
	       "BS2000Account"
               "ClearModuleList"
               "ContentDigest"
               "CoreDumpDirectory"
	       "DefaultType"
               "DocumentRoot"
               "EBCDICConvert"
               "EBCDICConvertByType"
	       "EBCDICKludge"
               "ErrorDocument"
               "ErrorLog"
               "FileETag"
               "Group"
	       "HostnameLookups"
               "IdentityCheck"
               "Include"
               "KeepAlive"
               "KeepAliveTimeout"
	       "LimitRequestBody"
               "LimitRequestFields"
               "LimitRequestFieldsize"
	       "LimitRequestLine"
               "Listen"
               "ListenBacklog"
               "LockFile"
               "LogLevel"
	       "MaxClients"
               "MaxKeepAliveRequests"
               "MaxRequestsPerChild"
               "MaxSpareServers"
	       "MinSpareServers"
               "NameVirtualHost"
               "Options"
               "PidFile"
               "Port"
               "Require"
	       "ResourceConfig"
               "RLimitCPU"
               "RLimitMEM"
               "RLimitNPROC"
               "Satisfy"
	       "ScoreBoardFile"
               "ScriptInterpreterSource"
               "SendBufferSize"
               "ServerAdmin"
	       "ServerAlias"
               "ServerName"
               "ServerPath"
               "ServerRoot"
               "ServerSignature"
	       "ServerTokens"
               "ServerType"
               "StartServers"
               "ThreadsPerChild"
	       "ThreadStackSize"
               "TimeOut"
               "UseCanonicalName"
               "User"
	       "AcceptPathInfo"                         ; apache2 core directives
               "ForceType"
               "LimitXMLRequestBody"
               "Require"
	       "SetHandler"
               "SetInputFilter"
               "SetOutputFilter"
	       "CoreDumpDirectory"                      ; apache2 mpm_common
               "Group"
               "Listen"
               "ListenBackLog"
               "LockFile"
	       "MaxClients"
               "MaxRequestsPerChild"
               "MaxSpareThreads"
	       "MaxThreadsPerChild"
               "MinSpareThreads"
               "NumServers"
               "PidFile"
	       "ScoreBoardFile"
               "SendBufferSize"
               "ServerLimit"
               "StartServers"
	       "StartThreads"
               "ThreadLimit"
               "ThreadsPerChild"
               "User"
	       "Listen"                                 ; mpm_netware
               "ListenBacklog"
               "MaxRequestsPerChild"
               "MaxSpareThreads"
	       "MaxThreads"
               "MinSpareThreads"
               "SendBufferSize"
               "StartThreads"
	       "ThreadStackSize"
	       "CoreDumpDirectory"                      ; mpm_winnt
               "Listen"
               "ListenBacklog"
	       "MaxRequestsPerChild"
               "PidFile"
               "SendBufferSize"
               "ThreadsPerChild"
	       "AssignUserId"                           ; mpm_perchild
               "ChildPerUserId"
               "CoreDumpDirectory"
               "Group"
	       "Listen"
               "ListenBacklog"
               "LockFile"
               "MaxRequestsPerChild"
	       "MaxSpareThreads"
               "MaxThreadsPerChild"
               "MinSpareThreads"
	       "NumServers"
               "PidFile"
               "ScoreBoardFile"
               "SendBufferSize"
	       "StartThreads"
               "User"
	       "AcceptMutex"                            ; mpm_prefork
               "CoreDumpDirectory"
               "Listen"
               "ListenBacklog"
	       "LockFile"
               "MaxRequestsPerChild"
               "MaxSpareServers"
	       "MaxSpareServers"
               "MinSpareServers"
               "MinSpareServers"
	       "PidFile"
               "ScoreBoardFile"
               "SendBufferSize"
	       "ServerLimit"
               "StartServers"
               "User"
	       "CoreDumpDirectory"                      ; mpm_worker
               "Group"
               "Listen"
               "ListenBacklog"
	       "LockFile"
               "MaxClients"
               "MaxRequestsPerChild"
	       "MaxSpareThreads"
               "MinSpareThreads"
               "PidFile"
	       "ScoreBoardFile"
               "SendBufferSize"
               "ServerLimit"
	       "StartServers"
               "ThreadLimit"
               "ThreadsPerChild"
               "User"
	       ;; Environment
	       "PassEnv"                                ; mod_env
               "SetEnv"
               "UnsetEnv"
	       "BrowserMatch"                           ; mod_setenvif
               "BrowserMatchNoCase"
               "SetEnvIf"
               "SetEnvIfNoCase"
	       ;; Content-Type decisions
	       "AddCharset"                             ; mod_mime
               "AddEncoding"
               "AddHandler"
               "AddLanguage"
               "AddType"
	       "DefaultLanguage"
               "ForceType"
               "RemoveEncoding"
               "RemoveHandler"
	       "RemoveType"
               "SetHandler"
               "TypesConfig"
	       "AddInputFilter"                         ; apache2 mod_mime
               "AddOutputFilter"
               "MultiviewsMatch"
	       "RemoveCharset"
               "RemoveInputFilter"
               "RemoveLanguage"
	       "RemoveOutputFilter"
	       "MimeMagicFile"                          ; mod_mime_magic
	       "CacheNegotiatedDocs"                    ; mod_negotiation
               "LanguagePriority"
	       "ForceLangaugePriority"                  ; apache2 mod_negotiation
	       ;; URL mapping
	       "Alias"                                  ; mod_alias
               "AliasMatch"
               "Redirect"
               "RedirectMatch"
               "RedirectTemp"
	       "RedirectPermanent"
               "ScriptAlias"
               "ScriptAliasMatch"
	       "RewriteEngine"                          ; mod_rewrite
               "RewriteOptions"
               "RewriteLog"
               "RewriteLogLevel"
	       "RewriteLock"
               "RewriteMap"
               "RewriteBase"
               "RewriteCond"
               "RewriteRule"
	       "UserDir"                                ; mod_userdir
	       "CheckSpelling"                          ; mod_speling
	       "VirtualDocumentRoot"                    ; mod_vhost_alias
               "VirtualDocumentRootIP"
               "VirtualScriptAlias"
	       "VirtualScriptAliasIP"
	       ;; Directory handling
	       "DirectoryIndex"                         ; mod_dir
	       "AddAlt"                                 ; mod_autoindex
               "AddAltByEncoding"
               "AddAltByType"
               "AddDescription"
               "AddIcon"
	       "AddIconByEncoding"
               "AddIconByType"
               "DefaultIcon"
               "FancyIndexing"
	       "HeaderName"
               "IndexIgnore"
               "IndexOptions"
               "IndexOrderDefault"
               "ReadmeName"
	       ;; Access control
	       "Allow"                                  ; mod_access
               "Deny"
               "Order"
	       "AuthGroupFile"                          ; mod_auth
               "AuthUserFile"
               "AuthAuthoritative"
	       "AuthDBMGroupFile"                       ; mod_auth_dbm
               "AuthDBMUserFile"
               "AuthDBMAuthoritative"
	       "AuthDBMType"                            ; apache2 mod_auth_dbm
	       "AuthDBGroupFile"                        ; mod_auth_db
               "AuthDBUserFile"
               "AuthDBAuthoritative"
	       "Anonymous"                              ; mod_auth_anon
               "Anonymous_Authoritative"
               "Anonymous_LogEmail"
	       "Anonymous_MustGiveEmail"
               "Anonymous_NoUserID"
               "Anonymous_VerifyEmail"
	       "AuthDigestFile"                         ; mod_auth_digest
               "AuthDigestGroupFile"
               "AuthDigestQop"
	       "AuthDigestNonceLifetime"
               "AuthDigestNonceFormat"
               "AuthDigestNcCheck"
	       "AuthDigestAlgorithm"
               "AuthDigestDomain"
	       "AuthDigestAlgorithm"                    ; apache2 mod_auth_digest
               "AuthDigestNcCheck"
               "AuthDigestNonceFormat"
	       "AuthDigestNonceLifetime"
	       "AuthDigestFile"                         ; mod_digest
	       ;; HTTP responses
	       "Header"                                 ; mod_headers
	       "RequestHeader"                          ; apache2 mod_headers
	       "MetaFiles"                              ; mod_cern_meta
               "MetaDir"
               "MetaSuffix"
	       "ExpiresActive"                          ; mod_expires
               "ExpiresByType"
               "ExpiresDefault"
	       ;; Dynamic content
	       "XBitHack"                               ; mod_include
	       "SSIEndTag"                              ; apache2 mod_include
               "SSIErrorMsg"
               "SSIStartTag"
               "SSITimeFormat"
	       "SSIUndefinedEcho"
	       "ScriptLog"                              ; mod_cgi
               "ScriptLogLength"
               "ScriptLogBuffer"
	       "Action"                                 ; mod_actions
               "Script"
	       "ISAPIReadAheadBuffer"                   ; mod_isapi WIN32 only
               "ISAPILogNotSupported"
               "ISAPIAppendLogToErrors"
	       "ISAPIAppendLogToQuery"
	       "ISAPIFileChache"                        ; apache2 mod_isapi
	       ;; Internal content handlers
	       "ExtendedStatus"                         ; mod_status
	       "AddModuleInfo"                          ; mod_info
	       ;; Logging
	       "CookieLog"                              ; mod_log_config
               "CustomLog"
               "LogFormat"
               "TransferLog"
	       "AgentLog"                               ; mod_log_agent
	       "RefererIgnore"                          ; mod_log_referer
               "RefererLog"
	       "CookieDomain"                           ; mod_usertrack
               "CookieExpires"
               "CookieName"
               "CookieStyle"
               "CookieTracking"
	       ;; Miscellaneous
	       "ImapMenu"                               ; mod_imap
               "ImapDefault"
               "ImapBase"
	       "ProxyRequests"                          ; mod_proxy
               "ProxyRemote"
               "ProxyPass"
               "ProxyPassReverse"
               "ProxyBlock"
	       "AllowCONNECT"
               "ProxyReceiveBufferSize"
               "ProxyIOBufferSize"
               "NoProxy"
	       "ProxyDomain"
               "ProxyVia"
               "CacheRoot"
               "CacheSize"
               "CacheMaxExpire"
	       "CacheDefaultExpire"
               "CacheLastModifiedFactor"
               "CacheGcInterval"
	       "CacheDirLevels"
               "CacheDirLength"
               "CacheForceCompletion"
               "NoCache"
	       "ProxyErrorOverride"                     ; apache2 mod_proxy
               "ProxyMaxForwards"
               "ProxyPreserveHost"
	       "ProxyRemote"
               "ProxyTimeout"
	       "LoadFile"                               ; mod_so
               "LoadModule"
	       "MMapFile"                               ; mod_mmap_static
	       ;; Development
               "Example"                                ; mod_example
	       ;; Obsolete directives
	       "BrowserMatch"                           ; mod_browser
               "BrowserMatchNoCase"
	       "CookieLog"                              ; mod_cookies
	       "LoadFile"                               ; mod_dld
               "LoadModule"
	       "TransferLog"                            ; mod_log_common
	       ;; Other stuff that I don't know which mod they belong in
	       "DefaultMode"
               "HTTPLogFile"
               "HTMLDir"
               "PrivateDir"
               "TopSites"
               "TopURLs"
               "LastURLs"
               "HeadPrefix"
               "HeadSuffix"
               "DocTitle"
               "DocTrailer"
               "HideURL"
               "HideSys"
	       ;; Apache2 extra builtin modules
	       "CharsetDefault"                         ; mod_charset_lite
               "CharsetOptions"
               "CharsetSourceEnc"
	       "AuthLDAPAuthoritative"                  ; mod_auth_ldap
               "AuthLDAPBindDN"
               "AuthLDAPBindPassword"
	       "AuthLDAPCompareDNOnServer"
               "AuthLDAPDereferenceAliases"
	       "AuthLDAPEnabled"
               "AuthLDAPFrontPageHack"
               "AuthLDAPGroupAttribute"
	       "AuthLDAPGroupAttributeIsDN"
               "AuthLDAPRemoteUserIsDN"
	       "AuthLDAPStartTLS"
               "AuthLDAPUrl"
	       "ScriptLog"                              ; mod_cgid
               "ScriptLogBuffer"
               "ScriptLogLength"
               "ScriptSock"
	       "ExtFilterDefine"                        ; mod_ext_filter
               "ExtFilterOptions"
	       "SuexecUserGroup"                        ; mod_suexec
	       "CacheFile"                              ; mod_file_cache
               "MMapFile"
	       "CacheDefaultExpire"                     ; mod_cache
               "CacheDisable"
               "CacheEnable"
	       "CacheIgnoreCacheControl"
               "CacheIgnoreNoLastMod"
	       "CacheLastModifiedFactor"
               "CacheMaxExpire"
               "CacheOn"
	       "Dav"                                    ; mod_dav
               "DavDepthInfinity"
               "DavLockDB"
               "DavMinTimeout"
	       "DeflateFilterNote"                      ; mod_deflate
               "DeflateMemLevel"
               "DeflateWindowSize"
	       "SSLCACertificateFile"                   ; mod_ssl
               "SSLCACertificatePath"
               "SSLCARevocationFile"
	       "SSLCARevocationPath"
               "SSLCertificateChainFile"
	       "SSLCertificateFile"
               "SSLCertificateKeyFile"
               "SSLCipherSuite"
	       "SSLEngine"
               "SSLLog"
               "SSLLogLevel"
               "SSLMutex"
               "SSLOptions"
	       "SSLPassPhraseDialog"
               "SSLProtocol"
               "SSLRandomSeed"
	       "SSLRequire"
               "SSLRequireSSL"
               "SSLSessionCache"
	       "SSLSessionCacheTimeout"
               "SSLVerifyClient"
               "SSLVerifyDepth"
	       "LDAPCacheEntries"                       ; mod_ldap
               "LDAPCacheTTL"
               "LDAPCertDBPath"
	       "LDAPOpCacheEntries"
               "LDAPOpCacheTTL"
               "LDAPSharedCacheSize"
               ;; Non-builtin Apache modules
               "SSLPassPhraseDialog"                    ; mod_ssl
               "SSLMutex"
               "SSLRandomSeed"
               "SSLSessionCache"
               "SSLSessionCacheTimeout"
               "SSLEngine"
               "SSLProtocol"
               "SSLCipherSuite"
               "SSLCertificateFile"
               "SSLCertificateKeyFile"
               "SSLCertificateChainFile"
               "SSLCACertificatePath"
               "SSLCACertificateFile"
               "SSLCARevocationPath"
               "SSLCARevocationFile"
               "SSLVerifyClient"
               "SSLVerifyDepth"
               "SSLLog"
               "SSLLogLevel"
               "SSLOptions"
               "SSLRequireSSL"
               "SSLRequire"
               "PerlAccessHandler"                      ; mod_perl 1 and 2
               "PerlAddVar"
               "PerlAuthenHandler"
               "PerlAuthzHandler"
               "PerlChildExitHandler"
               "PerlChildInitHandler"
               "PerlCleanupHandler"
               "PerlFixupHandler"
               "PerlHeaderParserHandler"
               "PerlInitHandler"
               "PerlLogHandler"
               "PerlModule"
               "PerlPassEnv"
               "PerlPostReadRequestHandler"
               "PerlRequire"
               "PerlSetEnv"
               "PerlSetVar"
               "PerlTypeHandler"
               "PerlDispatchHandler"                    ; mod_perl 1
               "PerlFreshRestart"
               "PerlHandler"
               "PerlOpmask"
               "PerlRestartHandler"
               "PerlScript"
               "PerlSendHeader"
               "PerlSetupEnv"
               "PerlTaintCheck"
               "PerlTransHandler"
               "PerlWarn"
               "PerlLoadModule"                         ; mod_perl 2
               "PerlOptions"
               "PerlSwitches"
               "PerlOpenLogsHandler"
               "PerlPostConfigHandler"
               "PerlPreConnectionHandler"
               "PerlProcessConnectionHandler"
               "PerlInputFilterHandler"
               "PerlOutputFilterHandler"
               "PerlSetInputFilter"
               "PerlSetOutputFilter"
               "PerlResponseHandler"
               "PerlInterpStart"
               "PerlInterpMax"
               "PerlInterpMinSpare"
               "PerlInterpMaxSpare"
               "PerlInterpMaxRequests"
               "PerlInterpScope"
               "PerlTrace"
               "PythonAccessHandler"                    ; mod_python
               "PythonAuthenHandler"
               "PythonAuthzHandler"
               "PythonAutoReload"
               "PythonCleanupHandler"
               "PythonConnectionHandler"
               "PythonDebug"
               "PythonEnablePdb"
               "PythonFixupHandler"
               "PythonHandler"
               "PythonHandlerModule"
               "PythonHeaderParserHandler"
               "PythonImport"
               "PythonInitHandler"
               "PythonInputFilter"
               "PythonInterpPerDirective"
               "PythonInterpPerDirectory"
               "PythonInterpreter"
               "PythonLogHandler"
               "PythonOptimize"
               "PythonOption"
               "PythonOutputFilter"
               "PythonPath"
               "PythonPostReadRequestHandler"
               "PythonTransHandler"
               "PythonTypeHandler"
               ;; Apache-SSL
               "SSLBanCipher"
               "SSLCACertificateFile"
               "SSLCACertificatePath"
               "SSLCacheServerPath"
               "SSLCacheServerPort"
               "SSLCacheServerRunDir"
               "SSLCertificateFile"
               "SSLCertificateKeyFile"
               "SSLCheckClientDN"
               "SSLDenySSL"
               "SSLDisable"
               "SSLEnable"
               "SSLEngineID"
               "SSLExportClientCertificates"
               "SSLFakeBasicAuth"
               "SSLKeyNoteTrustedAssertion"
               "SSLKeyNoteTrustedIssuerTemplate"
               "SSLNoCAList"
               "SSLRandomFile"
               "SSLRandomFilePerConnection"
               "SSLRequireCipher"
               "SSLRequireSSL"
               "SSLRequiredCiphers"
               "SSLSessionCacheTimeout"
               "SSLVerifyClient"
               "SSLVerifyDepth"

               ) :test 'string=))
           "\\)\\>")
	  1 'font-lock-keyword-face)

    (list (concat                                       ; values
	   "\\<\\("
           (regexp-opt
            (delete-duplicates
             '("allow"
               "deny"
               "on"
               "valid-user"
               "inetd"
               "standalone"
               "off"
               "user"
               "group"
               "any"
               "env"
               "mutual-failure"
               "full"
               "email"
               "force-response-1.0"
               "downgrade-1.0"
               "nokeepalive"
               "permanent"
               "temporary"
               "seeother"
               "gone"
               "All"
               "Options"
               "FileInfo"
               "AuthConfig"
               "Limit"
               "from"
               "None"
               "Basic"
               "Digest"
               "FancyIndexing"
               "IconsAreLinks"
               "ScanHTMLTitles"
               "SuppressLastModified"
               "SuppressSize"
               "SuppressDescription"
               "Minimal"
               "OS"
               "Full"
               "set"
               "append"
               "add"
               "unset"
               "none"
               "formatted"
               "semi-formatted"
               "unformatted"
               "error"
               "nocontent"
               "map"
               "referer"
               "URL"
               "inherit"
               "double"
               "GET"
               "PUT"
               "POST"
               "DELETE"
               "CONNECT"
               "OPTIONS"
               "Options"
               "Indexes"
               "Includes"
               "ExecCGI"
               "FollowSymLinks"
               "MultiViews"
               "IncludesNOEXEC"
               "SymLinksIfOwnerMatch"
               "uslock"
               "pthread"
               "sysvsem"
               "fcntl"
               "flock"
               "os2sem"
               "tpfcore"
               "default"
               "INode"
               "MTime"
               "builtin"                                ; mod_ssl
               "exec"
               "none"
               "file"
               "sem"
               "egd"
               "dbm"
               "on"
               "off"
               "shm"
               "shmht"
               "shmcb"
               "SSLv2"
               "SSLv3"
               "TLSv1"
               "All"
               "startup"
               "connect"
               "optional"
               "require"
               "optional_no_ca"
               "ssl-unclean-shutdown"
               "error"
               "warn"
               "info"
               "trace"
               "debug"
               "StdEnvVars"
               "CompatEnvVars"
               "ExportCertData"
               "FakeBasicAuth"
               "StrictRequire"
               "OptRenegotiate"
               "ssl-accurate-shutdown"
               "ssl-unclean-shutdown"
               "kRSA"                                   ; cipher stuff
               "kDHr"
               "kDHd"
               "kEDH"
               "aNULL"
               "aRSA"
               "aDSS"
               "aDH"
               "eNULL"
               "DES"
               "3DES"
               "RC4"
               "RC2"
               "IDEA"
               "MD5"
               "SHA1"
               "SHA"
               "EXP"
               "EXPORT40"
               "EXPORT56"
               "LOW"
               "MEDIUM"
               "HIGH"
               "RSA"
               "DH"
               "EDH"
               "ADH"
               "DSS"
               "NULL"
               "DES-CBC3-SHA"
               "DES-CBC3-MD5"
               "IDEA-CBC-SHA"
               "RC4-SHA"
               "RC4-MD5"
               "IDEA-CBC-MD5"
               "RC2-CBC-MD5"
               "RC4-MD5"
               "DES-CBC-SHA"
               "RC4-64-MD5"
               "DES-CBC-MD5"
               "EXP-DES-CBC-SHA"
               "EXP-RC2-CBC-MD5"
               "EXP-RC4-MD5"
               "EXP-RC2-CBC-MD5"
               "EXP-RC4-MD5"
               "NULL-SHA"
               "NULL-MD5"
               "ADH-DES-CBC3-SHA"
               "ADH-DES-CBC-SHA"
               "ADH-RC4-MD5"
               "EDH-RSA-DES-CBC3-SHA"
               "EDH-DSS-DES-CBC3-SHA"
               "EDH-RSA-DES-CBC-SHA"
               "EDH-DSS-DES-CBC-SHA"
               "EXP-EDH-RSA-DES-CBC-SHA"
               "EXP-EDH-DSS-DES-CBC-SHA"
               "EXP-ADH-DES-CBC-SHA"
               "EXP-ADH-RC4-MD5"
               "file"                                   ; Apache-SSL
               "egd"
               "IDEA-CBC-SHA"                           ; cipher stuff
               "NULL-MD5"
               "NULL-SHA"
               "EXP-RC4-MD5"
               "RC4-MD5"
               "RC4-SHA"
               "EXP-RC2-CBC-MD5"
               "IDEA-CBC-MD5"
               "EXP-DES-CBC-SHA"
               "DES-CBC-SHA"
               "DES-CBC3-SHA"
               "EXP-DH-DSS-DES-CBC-SHA"
               "DH-DSS-DES-CBC-SHA"
               "DH-DSS-DES-CBC3-SHA"
               "EXP-DH-RSA-DES-CBC-SHA"
               "DH-RSA-DES-CBC-SHA"
               "DH-RSA-DES-CBC3-SHA"
               "EXP-EDH-DSS-DES-CBC-SHA"
               "EDH-DSS-DES-CBC-SHA"
               "EDH-DSS-DES-CBC3-SHA"
               "EXP-EDH-RSA-DES-CBC"
               "EDH-RSA-DES-CBC-SHA"
               "EDH-RSA-DES-CBC3-SHA"
               "EXP-ADH-RC4-MD5"
               "ADH-RC4-MD"
               "EXP-ADH-DES-CBC-SHA"
               "ADH-DES-CBC-SHA"
               "ADH-DES-CBC3-SHA"
               "FZA-NULL-SHA"
               "FZA-FZA-CBC-SHA"
               "FZA-RC4-SHA"
               "DES-CFB-M1"
               "RC2-CBC-MD5"
               "DES-CBC-MD5"
               "DES-CBC3-MD5"
               "RC4-64-MD5"
               "NULL"
               "send-as-is"                             ; mod_asis
               "cgi-script"                             ; mod_cgi
               "imap-file"                              ; mod_imap
               "server-info"                            ; mod_info
               "isapi-isa"                              ; mod_isapi
               "ldap-status"                            ; mod_ldap
               "server-status"                          ; mod_status
               "On"                                     ; mod_perl
               "Off"
               "perl-script"
               "On"                                     ; mod_python
               "Off"
               "python-program"

               ) :test 'string=))
           "\\)\\>")
	  1 'font-lock-type-face)))
  "Expressions to highlight in `apache-mode' buffers.")

;; Syntax table
(if apache-mode-syntax-table
    nil
  (setq apache-mode-syntax-table (copy-syntax-table nil))
  (modify-syntax-entry ?_   "_"     apache-mode-syntax-table)
  (modify-syntax-entry ?-   "_"     apache-mode-syntax-table)
  (modify-syntax-entry ?\(  "(\)"   apache-mode-syntax-table)
  (modify-syntax-entry ?\)  ")\("   apache-mode-syntax-table)
  (modify-syntax-entry ?\<  "(\>"   apache-mode-syntax-table)
  (modify-syntax-entry ?\>  ")\<"   apache-mode-syntax-table)
  (modify-syntax-entry ?\"   "\""   apache-mode-syntax-table))


;;;###autoload
(defun apache-mode ()
  "Major mode for editing Apache configuration files.

\\{apache-mode-map}

\\[apache-mode] runs the hook `apache-mode-hook'."
  (interactive)
  (kill-all-local-variables)

  (setq mode-name "Apache")
  (setq major-mode 'apache-mode)
  (use-local-map apache-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(apache-font-lock-keywords nil t
                                                       ((?_ . "w")
                                                        (?- . "w"))))
  (set-syntax-table apache-mode-syntax-table)

  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#\\W*")
  (make-local-variable 'comment-column)
  (setq comment-column 48)
  (run-hooks 'apache-mode-hook))

;;;###autoload
(defun apache-set-file-patterns ()
  "Set file name patterns which automatically invoke `apache-mode'.
If you are using `apache-mode' installed separately from Emacs (i.e.
if its autoloads have not been automatically processed), then
add the following to your init file (see `user-init-file'):

  (add-to-list 'load-path \"/directory/where/it/is/installed\")
  (autoload 'apache-set-file-patterns \"apache-mode\")
  (apache-set-file-patterns)

The file name patterns are taken from the variable `apache-file-patterns'."
  (autoload 'apache-mode "apache-mode" "autoloaded" t)
  (mapcar (function (lambda (pat)
                      (add-to-list 'auto-mode-alist (cons pat 'apache-mode))))
          apache-file-patterns))

;;;###autoload
(apache-set-file-patterns)


;; Provides
(provide 'apache-mode)

;;; apache-mode.el ends here
