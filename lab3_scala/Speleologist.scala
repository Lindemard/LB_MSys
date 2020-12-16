package add

import add.Speleologist.StartRecieve
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

class Speleologist {

  private var navRef: ActorRef[Navigator.ActionRequest] = _
  private var envRef: ActorRef[Environment.Request] = _

  private var environmentBehaviorRef: ActorRef[Environment.Response] = _
  private var navigatorBehaviorRef: ActorRef[Navigator.ActionResponse] = _

  private var gameState: ActionResult = KeepGoing
  def setupActor(navRef: ActorRef[Navigator.ActionRequest], envRef: ActorRef[Environment.Request]): Behavior[StartRecieve] =
    Behaviors.receive((context, message) => {
      this.navRef = navRef
      this.envRef = envRef
      if (environmentBehaviorRef == null) {
        environmentBehaviorRef = context.spawn(environmentBehavior, "speleologist-behavior")
        navigatorBehaviorRef = context.spawn(navigatorBehavior, "speleologist-navigator")
      }
      Console.println("Спелеолог: изучает комнату")
      envRef ! Environment.EnvironmentRequest(environmentBehaviorRef)
      Behaviors.same
    })

  private def environmentBehavior: Behavior[Environment.Response] = Behaviors.receive[Environment.Response]((context, message) => {
    message match {
      case Environment.EnvironmentResponse(percept) =>
        Console.println("Спелеолог: информирует навигатора")
        navRef ! Navigator.ActionRequest(percept, "", navigatorBehaviorRef)
        Behaviors.same

      case Environment.ActionResponse(actionResult: ActionResult) =>
        this.gameState = actionResult
        Console.println("Спелеолог: статус ", actionResult)
        if(gameState == AgentDied) {
          {
            Console.println("Спелеолог мёртв")
            Behaviors.stopped
          }
          if(gameState == GotGold){
            Console.println("ПОБЕДА!")
            Behaviors.stopped
          }

          Console.println("Спелеолог: изучает комнату")
        }
        envRef ! Environment.EnvironmentRequest(environmentBehaviorRef)
        Behaviors.same
    }
  })

  private def navigatorBehavior: Behavior[Navigator.ActionResponse] = Behaviors.receive[Navigator.ActionResponse]((context, message) => {
    Console.println("Спелеолог: " + message.action)
    if(message.look ==LookUp){
      envRef ! Environment.PerformAction(message.action, Up, environmentBehaviorRef)
    }
    else if (message.look == LookRight){
      envRef ! Environment.PerformAction(message.action, Right, environmentBehaviorRef)
    }
    else if (message.look == LookLeft){
      envRef ! Environment.PerformAction(message.action, Left, environmentBehaviorRef)
    }
    else if (message.look == LookDown){
      envRef ! Environment.PerformAction(message.action, Down, environmentBehaviorRef)
    }


    Behaviors.same
  })

}
object Speleologist {
  case class StartRecieve()
}
