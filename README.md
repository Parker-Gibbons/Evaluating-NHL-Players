# Evaluating-NHL-Players
STAT 306 Topics in Statistics: Multivariate Sports Analytics - Project #3

Quantifying the Contribution of NHL Player Positions to Team Performance and How it affects Roster Construction

Kenyon College

Will Gibbs and Parker Gibbons 


Question of Interest: The basic question is: How does each position in hockey (forward, defenseman, and goalie) contribute to a team’s success in the NHL? NHL teams are faced with a salary cap that impacts how they can construct their roster and so being able to classify different player types and subsequently seeing how each player type contributes to team performance could illuminate how teams can best construct their roster. We would also like to compare if our findings can be generalized to the successful teams in the league or if each team has different thinking behind what positions they think contribute most to team performance.


Thoughts on Analysis: First, we plan on using a clustering technique (such as K-means or hierarchical clustering) to classify the different player types for each of the three positions. Then we plan on measuring how much each position contributes to team success in the regular season (can be measured by team points earned). We can do this by using tools such as linear regression (potentially with ridge/lasso/elastic nets) or Principal Component Analysis to provide insights on what positions contribute most to team performance and provide insights on ideal roster construction. 


Dataset: We will get most of our data from https://moneypuck.com/index.html and look at basic performance statistics as well as more advanced statistics. Our response variable will be team points earned over the regular season.

