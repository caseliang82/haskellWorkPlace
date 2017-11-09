The basic conceit of my project was to use Newton fractals to map literary styles of various works and authors into a visually interesting, information-rich format.  The multidimensional nature of modern authorship analysis techniques presents a particular challenge when it comes to representing and visualising data, but the amount of variation that can be coaxed out of functions with the Newton iteration method of generating fractals presents a particular opportunity to overcome some of those challenges.

I didn't know exactly how reliable the results would be, but because humans are so good at visual pattern recognition, I thought it would be interesting to see to what degree different authors are distinct.  Also, pigeons are good at visual pattern recognition, and I thought it was funny to think about pigeons being able to distinguish Tolstoy from Dostoevsky.   Due to my lack of access to trained pigeons, however, I was not able to pursue this line of inquiry.

To educate myself further about these two things, I referred mainly to two core documents.  To learn about Newton iterations and Newton fractals, I read:
http://www.chiark.greenend.org.uk/~sgtatham/newton/,
and to observe authorship analysis methods I looked at:
http://www.biostat.jhsph.edu/~rpeng/papers/archive/authorship-tas2-final.pdf.
I also acquainted myself with the standard Haskell library Data.Complex at:
https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Complex.html.
To render the images, I used Graphic.GD, the details of which can be found here:
https://hackage.haskell.org/package/gd-3000.7.3/docs/Graphics-GD.html.

I used variation in statistical aspects of the sample texts (mostly word frequency, as it is routinely found to be one of the best ways of finding variation amongst authors) to produce corresponding variation in the constants that made up the equation to which the Newton method was being applied.  The details of exactly how I mapped the former to the latter can be found in the source code; I used a variety of ways of developing the intermediary functions. For instance, I used Peng and Hengartner's paper to find approximations of the predictiveness of certain word frequencies, Google's Ngram Viewer to find the approximate average frequencies of words in written English, and a hand-made training corpus to estimate the amount of variation for each word.

To run the program, first pick a text to analyse and save it in the IO folder.  The text should be at least a few thousand words in order to generate meaningful results.  Next, load newton.hs, which should import the module Text from Text.hs.  Type:
	*Main > make “filepath”
where filepath is the location of the document for which you want to generate an image.  The image will take a few seconds (or even a few minutes) to generate, upon which time it will be saved in the IO folder.  

For example texts and images look in the examples folder.  I have examples from Robert Burns, Emily Dickinson, T. S. Eliot, David Hume, H. L. Mencken, William Shakespeare, and H. G. Wells.  I have organised it by author.  Upon cursory inspection, my attempt at predictably grouping the works of one author into similar fractals seems to have, for the most part, been successful.




--E. P. Hackett, November 2016

For any further questions or comments, contact me at s1681275@sms.ed.ac.uk.




