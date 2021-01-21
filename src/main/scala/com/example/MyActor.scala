package com.example

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.{Logging, LoggingAdapter}

import scala.util.Random

case class Greeter() extends Actor
{
	val log: LoggingAdapter = Logging(context.system, this)

	override def receive =
	{
		case "Mustafa Alhamoud" => log.info("Welcome, Mustafa Alhamoud")
		case "Łukasz Blachnicki" => log.info("Good morning, Łukasz Blachnicki")
		case "Tania Munthe" => log.info("Fine evening, Madam Tania")
		case _ => log.info("Hi I guess")
	}
}

case class Ball(points: Int)


case class PingPonger() extends Actor
{
	val log: LoggingAdapter = Logging(context.system, this)

	override def receive =
	{
		case Ball(points) =>
			if(points == 0)
			{
				log.info("starting")
				self ! Ball(points + 1)
			}
			else if(points > 9)
			{
				log.info("game over")
				context.system.terminate()
			}
			else if(Random.nextInt(2) > 0)
			{
				log.info("hit a ball with " + points + " points")
				sender ! Ball(points + 1)
			} else
			{
				log.info("missed a ball with " + points + " points")
				sender ! Ball(points)
			}
		case _ => log.info("what happened?")
	}
}

case class BallThrower() extends Actor
{
	val log: LoggingAdapter = Logging(context.system, this)

	override def receive =
	{
		case (Ball(points), guy1: ActorRef, guy2: ActorRef) =>
			log.info("" + points)
			if(points > 9)
			{
				log.info("game over")
				context.system.terminate()
			} else if(Random.nextInt(2) > 0) guy1 ! (Ball(points + 1), this.self, guy2)
				else guy2 ! (Ball(points + 1), this.self, guy1)
		case _ => log.info("what happened?")
	}
}


object main extends App
{

	//Task 1
	println("----------------------------Task 1----------------------------")
	val greetingSystem = ActorSystem("greetingSystem")
	var greeter = greetingSystem.actorOf(Props(Greeter()), name = "Greeter")
	greeter ! "Mustafa Alhamoud"
	greeter ! "Łukasz Blachnicki"
	greeter ! "Tania Munthe"
	greeter ! "Berke Oncu"
	greetingSystem.terminate

	Thread.sleep(1000);

	//Task 2
	println("\n\n\n----------------------------Task 2----------------------------\n\n\n")
	val pingpongSystem = ActorSystem("pingpongSystem");
	val pingPonger = pingpongSystem.actorOf(Props(PingPonger()), name = "PingPonger")
	pingPonger ! Ball(0)

	Thread.sleep(1000);

	//Task 3
	println("\n\n\n----------------------------Task 3----------------------------\n\n\n")
	val threeActorsSystem = ActorSystem("threeActorsSystem")
	val guy1 = threeActorsSystem.actorOf(Props(BallThrower()), name = "guy1")
	val guy2 = threeActorsSystem.actorOf(Props(BallThrower()), name = "guy2")
	val guy3 = threeActorsSystem.actorOf(Props(BallThrower()), name = "guy3")

	guy1 ! (Ball(0), guy2, guy3)
}