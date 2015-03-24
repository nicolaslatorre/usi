package multidimensionalscaling

class Document(val id: String, val title: String, val text: String, val tags: String, val date: String, val answerCount: Int, val commentCount: Int, val topicDistribution: List[(Int, Int)]) {

}