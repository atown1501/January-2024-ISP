End of ISP
	This ISP focused on exploring and cleaning an NFL play by play data set from 2009 to 2017 in the hopes of summarizing by drive. During this ISP, I have learned some useful skills in data analysis. One thing that I learned was how to change the value of the posteam and yrdline100 to the returning team when the PlayType is Punt. Some instances in variable PlayType proved troublesome to deal with, prompting me to remove Two Minute Warning, Timeout, Quarter End, and Half End. I also learned how to index specific values out of an entire dataset using the which function. Using the which function, I was able to check for discrepancies in the dataset to figure out if they were entry errors or just how the data was supposed to be entered. I also learned how to create a new variable in the dataset, in this case PlayType.new, which creates a new more detailed PlayType since Offensive Touchdown, Defensive Touchdown, and Interception were not play types in the original dataset. Towards the end of the ISP, we started to summarize each drive in a game, by grouping by GameID and Drive. I hope to be able to continue this analysis in the future.