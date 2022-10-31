package Report.GitPuller

import org.apache.logging.log4j.scala.Logging

object GitClient extends Logging {
  def assertThatGitIsInstalled: Boolean = {
    val gitVersion = scala.sys.process.Process("git --version").!
    gitVersion match {
      case 0 => logger.info("Git is installed")
        true
      case _ => logger.error("Git is not installed")
        false
    }
  }

  def getGitCloneRepo(repo: String, branch: String, repoPath: String): Unit = {
    require(repo.nonEmpty, "repo must not be empty")
    require(branch.nonEmpty, "branch must not be empty")
    require(repoPath.nonEmpty, "repoPath must not be empty")
    require(assertThatGitIsInstalled, "Git is not installed")

    val gitClone = scala.sys.process.Process(s"git clone -b $branch $repo $repoPath").!
    gitClone match {
      case 0 => logger.info(s"Cloned $repo to $repoPath")
      case _ => logger.error(s"Could not clone $repo to $repoPath")
    }
  }

  def getGitPullRepo(repoPath: String): Unit = {
    require(repoPath.nonEmpty, "repoPath must not be empty")
    require(assertThatGitIsInstalled, "Git is not installed")

    val gitPull = scala.sys.process.Process(s"git -C $repoPath pull").!
    gitPull match {
      case 0 => logger.info(s"Pulled $repoPath")
      case _ => logger.error(s"Could not pull $repoPath")
    }
  }


}
