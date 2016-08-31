/*
 * scala-exercises-client
 * Copyright (C) 2015-2016 47 Degrees, LLC. <http://www.47deg.com>
 */

package org.scalaexercises.client
package utils

import org.scalajs.dom.ext.KeyCode

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.raw.{ HTMLDivElement, HTMLElement, HTMLInputElement, HTMLTextAreaElement }
import org.scalajs.jquery.{ JQuery, jQuery ⇒ $ }
import fp.IO
import cats.data.OptionT
import cats.std.list._
import cats.std.option._
import cats.syntax.option._
import cats.syntax.traverse._

import scala.reflect.ClassTag

object DomHandler {

  import IO._

  sealed trait ExerciseElement {
    def setInputValue(value: String): IO[Unit]

    def value: String

    def attachKeyUpHandler(
      onkeyup:        (String, Seq[String]) ⇒ IO[Unit],
      onEnterPressed: String ⇒ IO[Unit]
    ): IO[Unit]

    protected def methodParent(input: HTMLElement): Option[String] = methodName($(input).closest(".exercise").getDiv)
  }

  trait ExerciseELementCompanion {
    /** @return All available [[ExerciseElement]]s.
      */
    def allElements: IO[List[ExerciseElement]]

    /** @param el Html element
      * @return All [[ExerciseElement]] under given html element.
      */
    def inputs(el: HTMLElement): List[ExerciseElement]

    // TODO What's different with 'inputs' function?
    def inputsInExercise(exercise: HTMLElement): Seq[ExerciseElement]
  }

  object TextInputExerciseElement extends ExerciseELementCompanion {
    override def allElements: IO[List[ExerciseElement]] = io { $(".exercise-code>input").inputs.map(_.asExerciseElement).toList }

    override def inputs(el: HTMLElement): List[ExerciseElement] = $(el).find("input").inputs.map(_.asExerciseElement).toList

    override def inputsInExercise(exercise: HTMLElement): Seq[ExerciseElement] = $(exercise).find("input").inputs.map(_.asExerciseElement)

    implicit class HtmlInputElementToExerciseELement(val elem: HTMLInputElement) extends AnyVal {
      def asExerciseElement: ExerciseElement = TextInputExerciseElement(elem)
    }
  }

  case class TextInputExerciseElement(elem: HTMLInputElement) extends ExerciseElement {

    override def setInputValue(value: String): IO[Unit] = for {
      _ ← io { $(elem) `val` (value) }
      _ ← setInputWidth(elem)
    } yield ()

    override def value: String = elem.value

    override def attachKeyUpHandler(
      onkeyup:        (String, Seq[String]) ⇒ IO[Unit],
      onEnterPressed: String ⇒ IO[Unit]
    ): IO[Unit] = io {
      $(elem).keyup((e: dom.KeyboardEvent) ⇒ {
        (for {
          _ ← OptionT(setInputWidth(elem) map (_.some))
          methodName ← OptionT(io(methodParent(elem)))
          exercise ← OptionT(io(findExerciseByMethod(methodName)))
          inputsValues = getInputsValues(exercise)
          _ ← OptionT((e.keyCode match {
            case KeyCode.Enter ⇒ onEnterPressed(methodName)
            case _             ⇒ onkeyup(methodName, inputsValues)
          }).map(_.some))
        } yield ()).value.unsafePerformIO()
      })
    }

    private def getInputsValues(exercise: HTMLElement): Seq[String] = inputsInExercise(exercise).map(_.value)

    private def inputsInExercise(exercise: HTMLElement): Seq[ExerciseElement] = TextInputExerciseElement.inputsInExercise(exercise)

  }

  object TextareaExerciseElement extends ExerciseELementCompanion {
    override def allElements: IO[List[ExerciseElement]] = io { $(".exercise-code>textarea").textareas.map(_.asExerciseElement).toList }

    override def inputs(el: HTMLElement): List[ExerciseElement] = $(el).find("textarea").textareas.map(_.asExerciseElement).toList

    override def inputsInExercise(exercise: HTMLElement): Seq[ExerciseElement] = $(exercise).find("textarea").textareas.map(_.asExerciseElement)

    implicit class HTMLTextAreaElementToExerciseELement(val elem: HTMLTextAreaElement) extends AnyVal {
      def asExerciseElement: ExerciseElement = TextareaExerciseElement(elem)
    }
  }

  case class TextareaExerciseElement(elem: HTMLTextAreaElement) extends ExerciseElement {
    override def setInputValue(value: String): IO[Unit] = for {
      _ ← io { $(elem) text (value) }
    } yield ()

    override def value: String = elem.value

    override def attachKeyUpHandler(onkeyup: (String, Seq[String]) ⇒ IO[Unit], onEnterPressed: (String) ⇒ IO[Unit]): IO[Unit] = io {
      $(elem).keyup((e: dom.KeyboardEvent) ⇒ {
        (for {
          methodName ← OptionT(io(methodParent(elem)))
          exercise ← OptionT(io(findExerciseByMethod(methodName)))
          inputValues = getInputsValues(exercise)
          _ ← OptionT((e.keyCode match {
            case KeyCode.Enter ⇒ io() // do nothing since 'Enter' is needed in textarea to go to next line
            case KeyCode.Ctrl  ⇒ io { println("CTRL detected") } // TODO remove
            case _             ⇒ onkeyup(methodName, inputValues)
          }).map(_.some))
        } yield ()).value.unsafePerformIO()
      })
    }

    // TODO in super
    private def getInputsValues(exercise: HTMLElement): Seq[String] = inputsInExercise(exercise).map(_.value)

    // TODO move from companion to here since no where other used
    private def inputsInExercise(exercise: HTMLElement): Seq[ExerciseElement] = TextareaExerciseElement.inputsInExercise(exercise)

  }

  /** Replaces text matched into html inputs
    */
  def replaceInputs(nodes: Seq[(HTMLElement, String)]): IO[Unit] = io {
    nodes foreach { case (n, r) ⇒ $(n).html(r) }
  }

  /** Highlights every preformatted code block.
    */
  def highlightCodeBlocks: IO[Unit] = io {
    $("pre").each((_: Any, code: dom.Element) ⇒ {
      js.Dynamic.global.hljs.highlightBlock(code)
    })
  }

  /** Converts emoji markup into inline emoji images.
    */
  def emojify: IO[Unit] = io {
    $(".modal-body").each((_: Any, el: dom.Element) ⇒ {
      js.Dynamic.global.emojify.run(el)
    })
  }

  /** Set the class attribute to an exercise node
    */
  def setExerciseClass(e: HTMLElement, style: String): IO[Unit] = io {
    $(e).attr("class", s"exercise $style")
  }

  /** Set the class attribute to an exercise code node
    */
  def setCodeClass(e: HTMLElement, style: String): IO[Unit] = io {
    $(e).attr("class", s"exercise-pre $style")
  }

  /** Write a message in the log of an exercise
    */
  def writeLog(e: HTMLElement, msg: String): IO[Unit] = io {
    $(e).find(".log").text(msg)
  }

  /** Assigns behaviors to the keyup event for inputs elements.
    */
  def onInputKeyUp(
    onkeyup:        (String, Seq[String]) ⇒ IO[Unit],
    onEnterPressed: String ⇒ IO[Unit]
  ): IO[Unit] = for {
    inputs ← allInputs
    _ ← inputs.map(input ⇒ input.attachKeyUpHandler(onkeyup, onEnterPressed)).sequence
  } yield ()

  /** Shows modal for signing up
    */
  def showSignUpModal: IO[Unit] = io($("#mustSignUp").modal("show"))

  def onButtonClick(onClick: String ⇒ IO[Unit]): IO[Unit] =
    allExercises.map(attachClickHandler(_, onClick)).sequence.map(_ ⇒ ())

  def attachClickHandler(exercise: HTMLElement, onClick: String ⇒ IO[Unit]): IO[Unit] = io {
    $(exercise).find(".compile button").click((e: dom.Event) ⇒ {
      onClick(getMethodAttr(exercise)).unsafePerformIO()
    })
  }

  def setInputWidth(input: HTMLInputElement): IO[JQuery] =
    io($(input).width(inputSize(getInputLength(input))))

  def inputReplacements: IO[Seq[(HTMLElement, String)]] = for {
    blocks ← getCodeBlocks
  } yield blocks.map(code ⇒ code → replaceInputByRes(getTextInCode(code)))

  def allExercises: List[HTMLDivElement] = {
    ($(".exercise").divs filter isMethodDefined).toList
  }

  def getMethodAttr(e: HTMLElement): String = $(e).attr("data-method").getOrElse("").trim

  def isMethodDefined(e: HTMLElement): Boolean = getMethodAttr(e).nonEmpty

  def library: Option[String] = $("body").attr("data-library").toOption

  def section: Option[String] = $("body").attr("data-section").toOption

  def libraryAndSection: Option[(String, String)] = for {
    lib ← library
    sec ← section
  } yield (lib, sec)

  def methods: List[String] = allExercises.map(getMethodAttr(_))

  def methodName(e: HTMLElement): Option[String] = Option(getMethodAttr(e)) filter (_.nonEmpty)

  def allInputs: IO[List[ExerciseElement]] = {
    import cats.implicits._
    TextInputExerciseElement.allElements |@| TextareaExerciseElement.allElements map (_ ++ _)
  }

  def inputs(el: HTMLElement): List[ExerciseElement] = TextInputExerciseElement.inputs(el) ++ TextareaExerciseElement.inputs(el)

  def findExerciseByMethod(method: String): Option[HTMLElement] = {
    allExercises.find(methodName(_) == Option(method))
  }

  def findExerciseCode(el: HTMLElement): Option[HTMLElement] = {
    $(el).find(".exercise-pre").all.headOption
  }

  def getCodeBlocks: IO[Seq[HTMLElement]] = io { $("code.exercise-code").elements }

  def getTextInCode(code: HTMLElement): String = $(code).text

  val resAssert = """(?s)(res\d+)""".r
  val resSizeAssert = """(?s)res(\d+)s(\d+)""".r
  val resLineAssert = """(?s)res(\d+)l(\d+)""".r

  def replaceInputByRes(text: String): String = {
    val result1 = resSizeAssert.replaceAllIn(text, """<input type="text" data-res="$1" size="$2"/>""")
    val result2 = resLineAssert.replaceAllIn(result1, """<textarea data-res="$1" rows="$2" cols="100"/>""")
    resAssert.replaceAllIn(result2, """<input type="text" data-res="$1"/>""")
  }

  def getInputLength(input: HTMLInputElement): Int = $(input).value.toString.length

  def isLogged: Boolean = $("#loggedUser").length > 0

  def inputSize(length: Int): Double = length match {
    case 0 ⇒ 12d
    case _ ⇒ (12 + (length + 1) * 7).toDouble
  }

  implicit class JQueryOps(j: JQuery) {

    def elements: Seq[HTMLElement] = all[HTMLElement]

    def divs: Seq[HTMLDivElement] = all[HTMLDivElement]

    def inputs: Seq[HTMLInputElement] = all[HTMLInputElement]

    def textareas: Seq[HTMLTextAreaElement] = all[HTMLTextAreaElement]

    def getDiv: HTMLDivElement = get[HTMLDivElement]

    def all[A <: dom.Element: ClassTag]: Seq[A] = j.toArray().collect { case d: A ⇒ d }

    def get[A <: dom.Element]: A = j.get().asInstanceOf[A]

  }

  @js.native
  trait BootstrapModal extends JQuery {
    def modal(action: String): BootstrapModal = js.native
  }

  implicit def jQueryToModal(jq: JQuery): BootstrapModal = jq.asInstanceOf[BootstrapModal]

}
