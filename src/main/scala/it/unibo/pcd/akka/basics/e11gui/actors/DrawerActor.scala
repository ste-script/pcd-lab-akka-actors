package it.unibo.pcd.akka.basics.e11gui.actors

import akka.actor.typed.Behavior
import it.unibo.pcd.akka.basics.e11gui.view.{
  DrawablePanel,
  DrawableRectangle,
  ElementFactory,
  SimpleDrawablePanel,
  SwingElementFactory
}

import java.awt.Color
import scala.concurrent.Future
import scala.swing.SimpleSwingApplication
import scala.swing.event.MousePressed
import scala.swing.{Dimension, Frame, MainFrame, SimpleSwingApplication}
import scala.util.Success
import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{Behaviors, TimerScheduler}
import it.unibo.pcd.akka.basics.e11gui.actors.TimerTick.Tick

import scala.concurrent.duration.*

enum DrawMessage:
  case Rectangle(x: Int, y: Int, width: Int, height: Int)

object DrawerActor:
  def apply[G](panel: DrawablePanel[G], elementFactory: ElementFactory[G]): Behavior[DrawMessage] =
    Behaviors.receiveMessage:
      case DrawMessage.Rectangle(x, y, width, height) =>
        panel.addElement(elementFactory.createRectangle(x, y, width, height))
        Behaviors.same

enum ClickActorListener:
  case Click(x: Int, y: Int)

enum TimerTick:
  case Tick()

object ClickActor:
  def apply[G](panel: DrawablePanel[G], drawer: akka.actor.typed.ActorRef[DrawMessage]): Behavior[ClickActorListener] =
    Behaviors.setup: context =>
      panel.whenClicked { case (x, y) =>
        context.pipeToSelf(Future.successful((x, y))) {
          case Success((x, y)) => ClickActorListener.Click(x, y)
          case _ => ClickActorListener.Click(0, 0)
        }
      }
      Behaviors.receiveMessage:
        case ClickActorListener.Click(x, y) =>
          context.log.info(s"Click at $x, $y")
          drawer ! DrawMessage.Rectangle(x, y, 5, 5)
          Behaviors.same

object RandomDrawerActor:
  def apply[G](drawer: akka.actor.typed.ActorRef[DrawMessage], panel: DrawablePanel[G]): Behavior[TimerTick] =
    def randomX = (Math.random() * 400).toInt
    def randomY = (Math.random() * 300).toInt
    Behaviors.receiveMessage:
      case TimerTick.Tick() =>
        drawer ! DrawMessage.Rectangle(randomX, randomY, 5, 5)
        Behaviors.same

object TimerActor:
  def apply[G](randomDrawerActor: akka.actor.typed.ActorRef[TimerTick]): Behavior[TimerTick] =
    Behaviors.withTimers(timers =>
      timers.startTimerWithFixedDelay(TimerTick.Tick(), 1.second)
      Behaviors.receiveMessage:
        case TimerTick.Tick() =>
          randomDrawerActor ! TimerTick.Tick()
          Behaviors.same
    )

object MainActor:
  def apply[G](panel: DrawablePanel[G], factory: ElementFactory[G]): Behavior[Nothing] =
    Behaviors.setup: context =>
      val drawer = context.spawn(DrawerActor[G](panel, factory), "drawer")
      val clickActor = context.spawn(ClickActor(panel, drawer), "clickActor")
      val randomDrawerActor = context.spawn(RandomDrawerActor(drawer, panel), "randomActor")
      val timerActor = context.spawn(TimerActor(randomDrawerActor), "timerActor")
      Behaviors.empty

object MyDrawingApp extends SimpleSwingApplication:
  val panel = SimpleDrawablePanel()
  val factory = SwingElementFactory()

  def top: Frame = new MainFrame:
    title = "Drawable Panel Example"
    preferredSize = new Dimension(400, 300)
    contents = panel

    // Adding some rectangles to the panel
  akka.actor.typed.ActorSystem(MainActor(panel, factory), "main")
