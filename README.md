# hyperbible
_The new revised international KJV-ASV-DRB-DBT-ERV-WBT-WEB-YET-AKJV-WNT version of_ Ecclesiastes.

<center><img src="https://lh3.googleusercontent.com/sFMN0wOS6DyYU5vgPxFknbRptQ-dZPCxvJmdKX7bmZF_pWJv3AH3Mxwt14tA5HSi_RzxddRzAkHhkRkTqoeq=w2880-h1598-rw" width="500"></center>

Each sentence in this translation of _Ecclesiastes_ is drawn uniformly at random from one of ten translations of the Bible on every refresh. Study the text. Learn it. Love it. Live it. Refresh the page and you'll never see it again.

Put slightly differently: this book is sampled from the distributions of possible understandings of _Ecclesiastes_, like a more systematic [Septuagint](https://en.wikipedia.org/wiki/Septuagint). You haven't really understood the text until you've studied the nuances of each of its variations.

Made for [National Novel Generation Month 2017](https://github.com/NaNoGenMo/2017), _Ecclesiastes (KJV-ASV-DRB-DBT-ERV-WBT-WEB-YET-AKJV-WNT)_ runs to 10k words per translation, but as it draws its strength by consuming ten (slightly) different translations of the Bible, the hyperobject weighs in at well above the threshold of 50k.

Much thanks to the King James Bible, American Standard Version, Douay-Rheims Bible, Darby Bible Translation, English Revised Version, Webster Bible Translation, World English Bible, Young's Literal Translation, American KJV, and Weymouth New Testament.

# Instructions

`bible.hs` is a Haskell file that generates the HTML for the page. You'll need to manually copy the Javascript at the top and bottom of the current `bible.html` into the generated `bible.html`, and edit the generated verses again.

`bible.html` contains the js to randomly sample verses from the different translations.

This repository contains the original corpus for the book, `ecc.txt`, as well as `ecc-nl.txt` if you want to just read all the translations at once. Credit for the full corpus—a truly amazing Excel spreadsheet of ten Bibles—goes to the site [hackathon.bible](http://hackathon.bible), specifically to [Biblehub](http://biblehub.net). (_N.B._ There is a parse error in the corpus in _Exodus_.) 

This book is made out of a spirit of respect and love, not sacrilege.
