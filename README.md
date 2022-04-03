# Large

Large is a Medium clone REST API written in the Typelevel stack.

Featuring:
- Cats Effect 3
- Tagless Final
- Cats Validation
- Monocle
- Refined Types
- Http4s
- Skunk

## Resources


### Users
`GET api/users/$id`

Sample Response:
```json
{
    "item": {
        "id": "ppUqJaBPP3ZpsexeHl7aT",
        "name": "Scala Luvr",
        "bio": "Here to share my experiences with Scala!",
        "email": {
            "address": "scala.luvr@email.com",
            "status": "Unverified"
        },
        "tier": "Premium",
        "followers": 21,
        "following": 15,
        "likedArticles": 18,
        "joinDate": "2022-04-01",
        "articles": [
          "YqNIt7BqQxlDbMuX2eux1",
          "sOGLMtenDiC7kQM-38hcK",
          "vdoxZhETeR6Ptxxv41P_K",
          "pW4kbqbV0BuKRlfWelQbm",
          "rdoFT8pdnutq1ylMtAA6r"
        ]
    }
}
```

`POST api/users`

Example of a good request:

```json
{
    "name": "Scala Luvr",
    "bio": "Just a lover of Scala and functional programming.",
    "email": "scala.luvr@email.com"
}
```

Response:
```json
{
    "item": {
            "id": "ppUqJaBPP3ZpsexeHl7aT",
            "name": "Scala Luvr",
            "bio": "Just a lover of Scala and functional programming.",
            "email": {
                "address": "scala.luvr@email.com",
                "status": "Unverified"
            },
            "tier": "Trial",
            "followers": 0,
            "following": 0,
            "likedArticles": 0,
            "joinDate": "2022-04-01",
            "articles": []
        }
}
```

Example of a bad request:
```json
{
    "name": "AnOverlyLongUserName",
    "bio": "",
    "email": "bad.email"
}
```

Response:
```json
{
    "items": [
        "Username must be less than or equal to 15 chars.",
        "Bio cannot be empty.",
        "Email must be in a valid format."
    ]
}
```
### Blogs

`GET api/blogs/$id`

Response:
```json
{
    "item": {
        "id": "rdoFT8pdnutq1ylMtAA6r",
        "title": "How to Use Map, Filter, and Fold",
        "content": "In this article, we're going to go over how use the big three higher order functions...",
        "author": "ppUqJaBPP3ZpsexeHl7aT",
        "word_count": 16,
        "reading_time": 0.08,
        "likes": 5,
        "visibility": "Public",
        "published_on": "2022-04-02",
        "last_updated": "2022-04-02"
    }
}
```
### Tags
