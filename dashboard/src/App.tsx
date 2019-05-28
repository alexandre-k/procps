import React from 'react';
import logo from './logo.svg';
import './App.css';

class App extends React.Component{
  componentDidMount() {
    fetch('http://127.0.0.1:5000/mprocesses')
      .then(res => res.json())
      .then(data => console.log(data))
      .catch(err => console.log(err))
  }
  render() {
    return (
        <div className="App">
        <header className="App-header">
            <img src={logo} className="App-logo" alt="logo" />
            <p>
            Edit <code>src/App.tsx</code> and save to reload.
            </p>
            <a
            className="App-link"
            href="https://reactjs.org"
            target="_blank"
            rel="noopener noreferrer"
            >
            Learn React
            </a>
        </header>
        </div>
        --- card :: MonitoredProcess -> Html
--- card mproc = do
---   div ! class_ "col s6 m6" $ do
---     div ! class_ "card blue-grey darken-1" $ do
---       div ! class_ "card-content white-text" $ do
---         span ! class_ "card-title" $ toMarkup (name mproc)
---         paragraph $ "PID: " ++ (pid (process mproc))
---         paragraph $ "Current state: " ++ (status mproc)
---         paragraph $"Log file: " ++ (logFile mproc)
---         paragraph $ "Memory usage: " ++ show (memoryUsage mproc)
---         paragraph $ "Uptime: " ++ show (uptime mproc)
---       div ! class_ "card-action" $ do
---         processState (status mproc)
---       where
---       processState :: String -> Html
---       processState "stopped" = do
---         a ! href "#" $ "start"
---       processState _ = do
---         a ! href "#" $ "restart"
---         a ! href "#" $ "stop"
---       paragraph :: T.Text -> Markup
---       paragraph txt = p $ toMarkup txt

    );
  }
}

export default App;
