package it.unibo.pps.ex2

enum Question:
  case Relevance, Significance, Confidence, Final

trait ConferenceReviewing:
  def loadReview(article: Int, relevanceScore: Int, significanceScore: Int, confidenceScore: Int, finalScore: Int): Unit
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles: Set[Int]
  def sortedAcceptedArticles: List[(Int, Double)]
  def averageWeightedFinalScoreMap: Map[Int, Double]

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
    database.getOrElse(article, List.empty).flatMap(map => map.get(question)).sorted

  def averageFinalScore(article: Int): Double = orderedScores(article, Question.Final) match
    case Nil => 0.0
    case s => s.sum.toDouble / s.size

  def acceptedArticles: Set[Int] =
    database.keys.filter(article => averageFinalScore(article) > 5 &&
                                    orderedScores(article, Question.Relevance).exists(_ >= 8)).toSet

  def sortedAcceptedArticles: List[(Int, Double)] =
    acceptedArticles.map(article => (article, averageFinalScore(article))).toList.sortBy(_._2)

  def averageWeightedFinalScoreMap: Map[Int, Double] =
    database.map {
      case (article, reviews) => (article, reviews.map(r => (r(Question.Confidence) * r(Question.Final)) / 10.0).sum / reviews.size)
    }


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

  val accepted = cr.acceptedArticles
  assert(accepted == Set(1, 2, 4), s"Failed acceptedArticles: expected Set(1, 2, 4), instead $accepted")

  val sortedArticles = cr.sortedAcceptedArticles
  assert(cr.sortedAcceptedArticles == List((4, 7.0), (2, 7.5), (1, 8.5)), s"Failed sortedAcceptedArticles: expected List((4, 7.0), (2, 7.5), (1, 8.5)), instead $sortedArticles")

  val weightedMap = cr.averageWeightedFinalScoreMap
  assert(math.abs(weightedMap(1) - 5.1) <= 0.01, s"Failed averageWeightedFinalScoreMap 1: weightedMap = $weightedMap")
  assert(math.abs(weightedMap(2) - 7.5) <= 0.01, s"Failed averageWeightedFinalScoreMap 2: weightedMap = $weightedMap")
  assert(math.abs(weightedMap(3) - 1.25) <= 0.01, s"Failed averageWeightedFinalScoreMap 3: weightedMap = $weightedMap")
  assert(math.abs(weightedMap(4) - 14.8 / 3) <= 0.01, s"Failed averageWeightedFinalScoreMap 4: weightedMap = $weightedMap")
  assert(math.abs(weightedMap(5) - 6.5) <= 0.01, s"Failed averageWeightedFinalScoreMap 5: weightedMap = $weightedMap")
  assert(weightedMap.size == 5, s"Wrong map size: expected 5, instead ${weightedMap.size}")