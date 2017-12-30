{
  host # ip of the dispatcher machine
, ekg-port ? 8000
, sshPort ? 22
, sshUser ? "root"
}:
with builtins;
let
  pkgs = import ../pkgs.nix { inherit config; };
  projectPackages = (import ../dependencies.nix { }).internalPackages;
  # Configure project packages
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskellPackages = projectPackages;
    };
  };
  configfile = pkgs.writeTextFile {
    name = "config.yaml";
    text = ''
        token: "433458637:AAEs7tbwD3bIOOmyV7oAr1q4lueoGo2P8-0"
        channel: -1001230156811
        time:
          - "07:00:00"
          - "10:00:00"
          - "16:00:00"
          - "22:00:00"
        stats_server: "http://localhost:${builtins.toString ekg-port}"
    '';
  };
  makeMachine = host:
    # Function that creates description of single iperf cluster node
    {
      host = host; # Host of machine to connect to via SSH
      port = sshPort; # Port of machine to connect to via SSH
      user = sshUser; # User of machine with passwordless sudo and allowed SSH login
      services = { # Set of systemd services to install and start
        hexiperf-monitoring-bot = { # key value is used as name of systemd unit file
          unit = pkgs.writeTextFile { # text of the systemd unit
            name = "hexiperf-monitoring-bot";
            text = ''
              [Unit]
              Description=Monitoring bot for hexiperf dispatcher
              After=networking.target
              StartLimitIntervalSec=60
              [Service]
              Restart=always
              KillSignal=SIGINT
              SuccessExitStatus=SIGINT
              ExecStart=${pkgs.haskellPackages.monitoring-telegram-bot}/bin/monitoring-telegram-bot \
                --conf ${configfile}
              User=root
              KillMode=process
              [Install]
              WantedBy=multi-user.target
              '';
          };
        };
      };
      backend = "Ubuntu"; # Which internal backend to use
    };
in {
  # Description of each remote machine
  machines = {
    node = makeMachine host;
  };
}
