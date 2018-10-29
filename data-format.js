[
  // This is the data from one post
  {
    keyword: String,
    platform: String,
    created_time: String,
    title: String,
    content: String,
    author: String,
    url: String,
    likes: Integer,
    views: Integer,
    comments: [
      // This is one comments
      { 
        comment_created_time: String,
        comment_author: String,
        comment_content: String
      },
      // ... followed by more comments
    ]
  }
  // ... followed by more posts
]


