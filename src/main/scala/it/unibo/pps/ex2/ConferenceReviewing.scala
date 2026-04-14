package it.unibo.pps.ex2

enum Question:
  case Relevance, Significance, Confidence, Final

trait ConferenceReviewing:
  def loadReview(article: Int, relevanceScore: Int, significanceScore: Int, confidenceScore: Int, finalScore: Int): Unit
  def loadReview(article: Int, scores: Map[Question, Int]): Unit

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
  val mapScore: Map[Question, Int] = Map(
    Question.Relevance -> 8,
    Question.Significance -> 8,
    Question.Confidence -> 7,
    Question.Final -> 8
  )
  cr.loadReview(4, mapScore)
  cr.loadReview(5, 6, 6, 6, 10)
  cr.loadReview(5, 7, 7, 7, 10)






