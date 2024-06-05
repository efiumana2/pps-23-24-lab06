package ex2

enum Question:
  case  RELEVANCE,      // "È importante per questa conferenza?"
        SIGNIFICANCE,   // "Produce contributo scientifico?"
        CONFIDENCE,     // "Ti senti competente a commentarlo?"
        FINAL           // "É un articolo da accettare?"

trait ConferenceReviewing {

    /**
     * @param article ID of article
     * @param scores  Scores of article
     * loads a review for the specified article, with complete scores as a map
     */
    def loadReview(article: Int, scores: Map[Question, Int]): Unit

    /**
     * @param article         ID of article
     * @param relevance       Relevance score of article
     * @param significance    Significance score of article
     * @param confidence      Confidence score of article
     * @param fin             Final score of article
     * loads a review for the specified article, with the 4 explicit scores
     */
    def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

    /**
     * @param article   ID of article
     * @param question  Question to query
     * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
     */
    def orderedScores(article: Int, question: Question): List[Int]

    /**
     * @param article ID of article
     * @return the average score to question FINAL taken by the specified article
     */
    def averageFinalScore(article: Int): Double

    /**
     * An article is considered accept if its averageFinalScore (not weighted) is > 5,
     * and at least one RELEVANCE score that is >= 8.
     *
     * @return the set of accepted articles
     */
    def acceptedArticles: Set[Int]

    /**
     * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
     */
    def sortedAcceptedArticles: List[(Int, Double)]

    /**
     * @return a map from articles to their average "weighted final score", namely,
     *         the average value of CONFIDENCE*FINAL/10
     *         Note: this method is optional in this exam
     */
    def averageWeightedFinalScoreMap: Map[Int, Double]
  }

class ConferenceReviewingImpl  extends ConferenceReviewing {

    private var reviews: List[(Int, Map[Question, Int])] = Nil

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit = //???
      if (scores.size < Question.values.length)
        throw IllegalArgumentException()
      reviews = reviews :+ (article, scores)

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit = //???
      var m = Map(Question.RELEVANCE -> relevance)
      m = m + (Question.SIGNIFICANCE -> significance)
      m = m + (Question.CONFIDENCE -> confidence)
      m = m + (Question.FINAL -> fin)
      reviews = reviews :+ (article, m)

    override def orderedScores(article: Int, question: Question): List[Int] = //???
      var l: List[Int] = Nil
      reviews.filter { case (a, _) => a == article }.map { case (_, p) => p(question) }.sorted

    override def averageFinalScore(article: Int): Double = //???
      var l: List[Int] = Nil
      l = reviews.filter { case (a, _) => a == article }.map { case (_, p) => p(Question.FINAL) }
      l.sum.toDouble / l.length

    private def accepted(article: Int): Boolean = //???
      averageFinalScore(article) > 5.0 &&
        reviews.filter { case (a, _) => a == article }.map { case (_, p) => p }.toSet.exists(p => p(Question.RELEVANCE) >= 0)

    override def acceptedArticles: Set[Int] = //???
      reviews.map{ case (a, _) => a }.filter(this.accepted).sorted.toSet

    override def sortedAcceptedArticles: List[(Int, Double)] = //???
      this.acceptedArticles.map(e => (e,this.averageFinalScore(e))).toList

    private def  averageWeightedFinalScore(article: Int):Double = //???
      var l: List[Double] = Nil
      l = reviews.filter { case (a, _) => a == article }.map { case (_, p) => p(Question.FINAL) * p(Question.CONFIDENCE) / 10.0}
      l.sum / l.length

    override def averageWeightedFinalScoreMap: Map[Int, Double] = //???
      reviews.map { case (a, _) => a}.map (aa => (aa,this.averageWeightedFinalScore(aa))).toMap

  }

@main def ppp(): Unit =

  val c = new ConferenceReviewingImpl

  // si ricordi che l'ordine delle domande è: relevance, significance, confidence, final
  c.loadReview(1, 8, 8, 6, 8)
  c.loadReview(1, 9, 9, 6, 9); // 5.4
  c.loadReview(2, 9, 9, 10, 9); // 9.0
  c.loadReview(2, 4, 6, 10, 6); // 6.0
  c.loadReview(3, 3, 3, 3, 3); // 0.9
  c.loadReview(3, 4, 4, 4, 4); // 1.6
  c.loadReview(4, 7, 7, 8, 7); // 5.6
  c.loadReview(4, 6, 6, 6, 6); // 3.6

  println(c.orderedScores(4,Question.CONFIDENCE))
  println(c.averageFinalScore(2))
  println(c.acceptedArticles)
  println(c.sortedAcceptedArticles)
  println(c.averageWeightedFinalScoreMap)