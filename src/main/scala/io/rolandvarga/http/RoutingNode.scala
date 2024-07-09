package io.rolandvarga.http

import io.rolandvarga.logger.Logger

case class RoutingNode(
  segment: String,
  handler: Handler,
  children: collection.mutable.Map[String, RoutingNode],
)

object Routes {
  def traverse(root: RoutingNode, input: String)(segmentOp: (RoutingNode, String) => RoutingNode): Option[RoutingNode] = {
    var current = root

    segmentsOpt(input) match {
      case segments => {
        if (segments.length == 0) { return None }

        segments.foreach(segment => {
          current.children.get(segment) match {
            case Some(child) => current = child
            case None => {
              current = segmentOp(current, segment)
            }
          }
        })
      } case _ => {
        Logger.error(s"invalid search input => $input")
        return None
      }
    }
    Some(current)
  }

  def search(root: RoutingNode, input: String): Option[RoutingNode] = {
    traverse(root, input) { (_, _) =>
      return None
    }
  }

  def insert(root: RoutingNode, input: String, handler: Handler): Option[RoutingNode] = {
    val child = traverse(root, input) { (current, segment) =>
      current.children(segment) =
        RoutingNode(segment, handler, collection.mutable.Map.empty[String, RoutingNode])
      current.children(segment)
    }
    Option.when(child.isDefined)(root)
  }

  def segmentsOpt(input: String): Array[String] = {
    input.split(" ") match {
      case Array(method, path) => {
        Array(method) ++ path.split("/").filter(_.nonEmpty)
      }
      case _ => {
        Logger.error(s"invalid search input => $input")
        Array.empty
      }
    }
  }
}