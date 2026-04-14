package it.unibo.pps.ex2

enum Question:
  case Relevance, Significance, Confidence, Final

trait ConferenceReviewing:
  def loadReview(article: Int, relevanceScore: Int, significanceScore: Int, confidenceScore: Int, finalScore: Int): Unit
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double

class ConferenceReviewingImpl extends ConferenceReviewing:

  private var database: Map[Int, List[Map[Question, Int]]] = Map.empty

  def loadReview(article: Int, relevanceScore: Int, significanceScore: Int, confidenceScore: Int, finalScore: Int): Unit =
    val scoresMap = Map(Question.Relevance -> relevanceScore,
                        Question.Significance -> significanceScore,
                        Question.Confidence -> confidenceScore,
                        Question.Final -> finalScore)
    loadReview(article, scoresMap)

  def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    val oldReviews = database.getOrElse(article, List.empty)
    val newReviews = scores :: oldReviews
    database += (article -> newReviews)

  def orderedScores(article: Int, question: Question): List[Int] =
    val reviews = database.getOrElse(article, List.empty)
    reviews.flatMap(map => map.get(question)).sorted

  def averageFinalScore(article: Int): Double = orderedScores(article, Question.Final) match
    case Nil => 0.0
    case s => s.sum.toDouble / s.size


@main def conferenceReviewingImplTest(): Unit =
  val cr = ConferenceReviewingImpl()
  cr.loadReview(1, 8, 8, 6, 8)
  cr.loadReview(1, 9, 9, 6, 9)
  cr.loadReview(2, 9, 9, 10, 9)
  cr.loadReview(2, 4, 6, 10, 6)
  cr.loadReview(3, 3, 3, 3, 3)
  cr.loadReview(3, 4, 4, 4, 4)
  cr.loadReview(4, 6, 6, 6, 6)
  cr.loadReview(4, 7, 7, 8, 7)
  val mapScore = Map(
    Question.Relevance -> 8,
    Question.Significance -> 8,
    Question.Confidence -> 7,
    Question.Final -> 8
  )
  cr.loadReview(4, mapScore)
  cr.loadReview(5, 6, 6, 6, 10)
  cr.loadReview(5, 7, 7, 7, 10)

  assert(cr.orderedScores(2, Question.Relevance) == List(4, 9), "Failed article 2 Relevance")
  assert(cr.orderedScores(4, Question.Confidence) == List(6, 7, 8), "Failed article 4 Confidence")
  assert(cr.orderedScores(5, Question.Final) == List(10, 10), "Failed article 5 Final")

  assert(math.abs(cr.averageFinalScore(1) - 8.5) <= 0.01, "Failed 1")
  assert(math.abs(cr.averageFinalScore(2) - 7.5) <= 0.01, "Failed 2")
  assert(math.abs(cr.averageFinalScore(3) - 3.5) <= 0.01, "Failed 3")
  assert(math.abs(cr.averageFinalScore(4) - 7.0) <= 0.01, "Failed 4")
  assert(math.abs(cr.averageFinalScore(5) - 10.0) <= 0.01, "Failed 5")

