package main;


import org.json.JSONArray;
import org.json.JSONObject;

import solver.PathFinder;
import solver.TaskSolver;
import units.SourceStream;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


public class Main {

	public static final String TAG = "release-candidate";

	private static ExecutorService threadPool = null;
	public static ExecutorService getThreadPool() {
	    return threadPool;
	}

    public static void main(String[] args) throws IOException {
    	long starttime = System.currentTimeMillis();
    	Statistics stats = new Statistics();
        List<Solution> bestSolutions = new ArrayList<>();
    	threadPool = Executors.newFixedThreadPool(6);
    	try {
    	    Arguments arguments = processArgs(args);
    	    List<JSONObject> problems = initProblems(arguments);
    	    if (problems.isEmpty()) return; //todo warn user that no files were given? or just used for translation
    	    int maxlevel = arguments.level;
    	    if (maxlevel == -1) maxlevel = PathFinder.Mode.getMaxLevel();
    	    long levelAverage = 0;
    	    boolean broken = false;
    	    for (int level = 0; level <= maxlevel; level++) {
    	        int seedcount = 0;
    	        long levelstart = System.currentTimeMillis();
                List<Solution> solutions = new ArrayList<>();
    	        problemLoop: for (JSONObject problem : problems) {
    	            int id = problem.getInt("id");
    	            for (SourceStream stream : SourceStream.getSourceStreams(problem)) {
    	                seedcount ++;
    	                if (timeRunsOut(arguments.secondsTimeLimit, starttime, levelstart, solutions.size(), levelAverage)) {
    	                    //todo inform stats about having broken the loop
    	                    broken = true;
    	                    break problemLoop;
    	                }
    	                long start = System.currentTimeMillis();
    	                Boardstate board = new Boardstate(problem);
    	                Solution solution = new TaskSolver(board, stream, arguments.getPowerWords()).solve(level);
    	                solution.id = id;
    	                long time = System.currentTimeMillis() - start;
    	                solution.seconds = (int) Math.ceil(time / 1000);
    	                solutions.add(solution);
    	            }
    	        }
    	        int secondsLevel = (int) Math.ceil(System.currentTimeMillis() - levelstart) / 1000;
                int secondsTotal = (int) Math.ceil(System.currentTimeMillis() - starttime) / 1000;
    	        if (arguments.devMode) System.out.println("level " + level + " completed: " + secondsLevel + " seconds (" + secondsTotal + " seconds total)");
    	        if (arguments.stats) stats.addSubSolutions(level, secondsLevel, solutions);   	        
    	        bestSolutions = filterBestSolutions(bestSolutions, solutions);
    	        levelAverage = (System.currentTimeMillis() - levelstart) / seedcount;
    	        if (broken) break; // do not start new level if we ran out of time
    	    }
    	    
    	    if (arguments.stats) stats.set(bestSolutions);
    	    createJsonOutput(bestSolutions, arguments.devMode);
    	    int duration = (int) Math.ceil((System.currentTimeMillis() - starttime) / 1000);
    	    if (arguments.stats) stats.writeStatsFile("results/stats.txt", duration);
    	}
    	finally {
    	    if (threadPool != null) {
    	        threadPool.shutdown();
    	    }
    	}
    }

    private static boolean timeRunsOut(int secondsTimeLimit, long starttime, long levelstart, int size, long previousAverage) {
        long totalElapsed = System.currentTimeMillis() - starttime;
        long totalRemaining = (secondsTimeLimit * 1000) - totalElapsed;
        //if we start a new level, if we have less then ten times the average time for the previous level, time runs out
        if (size == 0) return (totalRemaining < (previousAverage * 10));
                
        long levelElapsed = System.currentTimeMillis() - levelstart;
        long levelAverage = levelElapsed / size;
        //if we have less than three times the average time needed for a seed left, we say time runs out
        return (totalRemaining < (levelAverage * 3));
    }

    private static List<Solution> filterBestSolutions(List<Solution> bestSolutions, List<Solution> solutions) {
        if (bestSolutions.isEmpty()) return new ArrayList<>(solutions);
        List<Solution> newBest = new ArrayList<>();
        int i = 0;
        //replace old ones with better ones
        for (; i < solutions.size(); i++) {
            Solution oldsol = bestSolutions.get(i);
            Solution newsol = solutions.get(i);
            if (newsol.points > oldsol.points) {
                newBest.add(newsol);
            } else {
                newBest.add(oldsol);
            }
        }
        //if new solutions is not complete (break due to time) take old ones for missing problems
        for (; i < bestSolutions.size(); i++) {
            newBest.add(bestSolutions.get(i));
        }
        return newBest;
    }

    private static List<JSONObject> initProblems(Arguments arguments) throws IOException {
        List<JSONObject> results = new ArrayList<>();
        for (String fileString : arguments.getFiles()) {
            String jsonString = new String(Files.readAllBytes(Paths.get(fileString)));
            JSONObject json = new JSONObject(jsonString);
            results.add(json);
        }
        return results;
    }

    private static void createJsonOutput(List<Solution> solutions, boolean writefile) {
        JSONArray combined = new JSONArray();
        for (Solution solution : solutions) {
            JSONObject output = new JSONObject();
            output.put("problemId", solution.id);
            output.put("seed", solution.seed);
            output.put("tag", TAG);
            output.put("solution", solution.commandString);
            combined.put(output);
        }
        String file = solutions.size() + "_results_" + TAG;
        if (writefile) writeOutputFile(combined.toString(), file);
        if (!writefile) System.out.println(combined.toString());
    }

    private static void writeOutputFile(String result, String filename) {
        String fileName = "results/" + filename + ".json";
        PrintWriter writer;
        try {
            writer = new PrintWriter(fileName, "UTF-8");
        } catch (FileNotFoundException | UnsupportedEncodingException e) {
                throw new RuntimeException(e);
        }
        writer.println(result);
        writer.close();
    }

    private static Arguments processArgs(String[] args) {
        List<String> files = new ArrayList<String>();
        List<String> powerWords = new ArrayList<String>();
        int seconds = 60 * 60 * 12;
        int megabytes = 0;
        boolean devmode = false;
        boolean stats = false;
        int cores = 0;
        int level = -1;
        for (int i = 0; i < args.length - 1; i += 2) {
            String flag = args[i];
            String value = args[i + 1];
            switch (flag) {
                case "-c" :     cores = Integer.parseInt(value);
                                break;
                case "-dev" :   devmode = Boolean.parseBoolean(value);
                                break;
                case "-stats" : stats = Boolean.parseBoolean(value);
                                break;
                case "-f" :     files.add(value);
                                break;
                case "-t" :     seconds = Integer.parseInt(value);
                                break;
                case "-m" :     megabytes = Integer.parseInt(value);
                                break;
                case "-p" :     powerWords.add(value.replace("\"", ""));
                                break;
                case "-trans" : System.out.println(value + ": " + commands.Command.translate(value.toLowerCase()));
                                break;
                case "-l" :     level = Integer.parseInt(value);
                                break;
                case "--delay":
                    // delay for attaching profile
                    try {
                        Thread.sleep(30000);
                    } catch (InterruptedException e) {
                        // can ignore here
                    }
                    break;
                default:
                    System.err.println("warning: unknown argument " + flag);
                    break;
            }
        }
        return new Arguments(files, powerWords, seconds, megabytes, devmode, stats, cores, level);
    }

    private static class Arguments {
        private final List<String> files;
        private final List<String> powerWords;
        public final int secondsTimeLimit;
        public final boolean devMode;
        public final boolean stats;
        public final int cores;
        public final int level;

        public Arguments(List<String> files, List<String> powerWords, int seconds, int megabytes, boolean devmode, boolean stats, int cores, int level) {
            this.files = new ArrayList<String>(files);
            this.powerWords = new ArrayList<String>(powerWords);
            if (seconds > 10) seconds -= 3; //safety margin
            this.secondsTimeLimit = seconds;
            this.devMode = devmode;
            this.stats = stats;
            this.cores = cores;
            this.level = level;
        }

        public List<String> getFiles() {
            return new ArrayList<String>(files);
        }

        public List<String> getPowerWords() {
            return new ArrayList<String>(powerWords);
        }
    }
}
