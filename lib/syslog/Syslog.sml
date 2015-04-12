structure Syslog =
struct
val op << = Word.<<
infix 5 <<

(*
 * priorities/facilities are encoded into a single 32-bit quantity, where the
 * bottom 3 bits are the priority (0-7) and the top 28 bits are the facility
 * (0-big number).  Both the priorities and the facilities map roughly
 * one-to-one to strings in the syslogd(8) source code.  This mapping is
 * included in this file.
 *
 * priorities (these are ordered)
 *)
type loglevel = int
val EMERG   : loglevel = 0 (* system is unusable *)
val ALERT   : loglevel = 1 (* action must be taken immediately *)
val CRIT    : loglevel = 2 (* critical conditions *)
val ERR     : loglevel = 3 (* error conditions *)
val WARNING : loglevel = 4 (* warning conditions *)
val NOTICE  : loglevel = 5 (* normal but significant condition *)
val INFO    : loglevel = 6 (* informational *)
val DEBUG   : loglevel = 7 (* debug-level messages *)

(* option *)
type openflag = word
val PID    : openflag = 0wx01 (* log the pid with each message *)
val CONS   : openflag = 0wx02 (* log on the console if errors in sending *)
val ODELAY : openflag = 0wx04 (* delay open until first syslog() (default) *)
val NDELAY : openflag = 0wx08 (* don't delay open *)
val NOWAIT : openflag = 0wx10 (* don't wait for console forks: DEPRECATED *)
val PERROR : openflag = 0wx20 (* log to stderr as well *)

fun optionsToWord options =
  List.foldl Word.orb 0w0 options

(* facility codes *)
type facility = word
val KERN     : facility = (0w0<<0w3)  (* kernel messages *)
val USER     : facility = (0w1<<0w3)  (* random user-level messages *)
val MAIL     : facility = (0w2<<0w3)  (* mail system *)
val DAEMON   : facility = (0w3<<0w3)  (* system daemons *)
val AUTH     : facility = (0w4<<0w3)  (* security/authorization messages *)
val SYSLOG   : facility = (0w5<<0w3)  (* messages generated internally by syslogd *)
val LPR      : facility = (0w6<<0w3)  (* line printer subsystem *)
val NEWS     : facility = (0w7<<0w3)  (* network news subsystem *)
val UUCP     : facility = (0w8<<0w3)  (* UUCP subsystem *)
val CRON     : facility = (0w9<<0w3)  (* clock daemon *)
val AUTHPRIV : facility = (0w10<<0w3) (* security/authorization messages (private) *)
val FTP      : facility = (0w11<<0w3) (* ftp daemon *)

(* other codes through 15 reserved for system use *)
val LOCAL0   : facility = (0w16<<0w3) (* reserved for local use *)
val LOCAL1   : facility = (0w17<<0w3) (* reserved for local use *)
val LOCAL2   : facility = (0w18<<0w3) (* reserved for local use *)
val LOCAL3   : facility = (0w19<<0w3) (* reserved for local use *)
val LOCAL4   : facility = (0w20<<0w3) (* reserved for local use *)
val LOCAL5   : facility = (0w21<<0w3) (* reserved for local use *)
val LOCAL6   : facility = (0w22<<0w3) (* reserved for local use *)
val LOCAL7   : facility = (0w23<<0w3) (* reserved for local use *)


val openlog' = _import "openlog":  (string, word, word) -> ()
val syslog' = _import "syslog" : (int, string) -> ()

fun openlog(ident, options , facility: facility) = openlog'(ident, optionsToWord options, facility)
fun syslog(loglevel: loglevel, msg) = syslog'(loglevel, msg)
val closelog = _import "closelog": () -> ()
end
