
Usage="
$0 <event> <o-df path>

or

$0 -
for reading stdin and agregating the results.

Event can be Online, Missing or Lost.
Modify this script for IFTTT key and other configuration.
"


#EMAIL=

IFTTTMakerKey=''



# $1 is seperator, others are array elements
function join_by { local d=$1; shift; echo -n "$1"; shift; printf "%s" "${@/#/$d}";  }
function unquote { local temp="${1%\"}"; temp="${temp#\"}"; echo "$temp"; }

NL='\n'


trigger_mail(){
    echo "Subject: $ObjectPath is now $Event" | sendmail $EMAIL
}


trigger_ifttt(){
    set -x
    Return=`curl -X POST -H "Content-Type: application/json" \
        -d '{"value1":"'"$ObjectPath"'","value2":"'"$ObjectURL"'","value3":"'"$Reason"'"}' \
        "https://maker.ifttt.com/trigger/$Event/with/key/$IFTTTMakerKey" \
        -s`
    set +x
    if [[ "$Return" != "Congratulations! You've fired the $Event event" ]]; then
        echo "$Return"
    fi
}

RootUrl="https://otaniemi3d.cs.hut.fi/omi/node/"
if [[ "$1" == "-" ]]; then
    declare -a Onlines
    declare -a Missings
    declare -a Losts

    while read line; do
        data=($line)
        Event=${data[0]}
        ObjectPath=`unquote ${data[1]}`
        case $Event in
            Online)
                Onlines=("${Onlines[@]}" "$ObjectPath")
                ;;
            Missing)
                Missings=("${Missings[@]}" "$ObjectPath")
                ;;
            Lost)
                Losts=("${Losts[@]}" "$ObjectPath")
                ;;
        esac
    done


    ObjectPath=${Onlines[0]}
    if [[ -n $ObjectPath ]]; then
        ObjectURL="$RootUrl$ObjectPath"
        Event=Online
        Reason='ONLINE:\n'`join_by $NL "${Onlines[@]}"`
        trigger_ifttt
        sleep 0.5s
    fi

    ObjectPath=${Missings[0]}
    if [[ -n $ObjectPath ]]; then
        ObjectURL="$RootUrl$ObjectPath"
        Event=Missing
        Reason='MISSING:\n'`join_by $NL "${Missings[@]}"`
        trigger_ifttt
        sleep 0.5s
    fi

    ObjectPath=${Losts[0]}
    if [[ -n $ObjectPath ]]; then
        ObjectURL="$RootUrl$ObjectPath"
        Event=Lost
        Reason='LOST:\n'`join_by $NL "${Losts[@]}"`
        trigger_ifttt
        sleep 0.5s
    fi
else
    ObjectPath=$2
    ObjectURL="$RootUrl$ObjectPath"
    Event=$1
    case $Event in
        Online)
            Reason='New data detected. Starting to watch after one hour or earlier if stable.'
            ;;
        Missing)
            Reason='Not seen at least in `avarage + 3 * stdDeviation` (or special cases).'
            ;;
        Lost)
            Reason='Was lost one day ago.'
            ;;
    esac
    if [[ -n $Event ]]; then
        trigger_ifttt
        sleep 1s
    fi
fi



