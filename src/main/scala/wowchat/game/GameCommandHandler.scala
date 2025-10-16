package wowchat.game

trait GameCommandHandler {

  def sendMessageToWow(tp: Byte, message: String, target: Option[String])
  def sendNotification(message: String)

  def handleWho(arguments: Option[String]): Option[String]
  def handleGmotd(): Option[String]
  def handleGuildInvite(target: String): Option[String]
  def handleGuildKick(target: String): Option[String]

  def handleGuildPromote(target: String): Option[String]

  def handleGuildDemote(target: String): Option[String]
}
